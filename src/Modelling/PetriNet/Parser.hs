{-|
A module for parsing Petri Alloy instances into Haskell representations defined
by the 'Modelling.PetriNet.Types' module.
The instances contain valid and invalid Petri nets that is why these are parsed
into types as 'PetriLike' which allow representing some invalid representations
of graphs which are similar to Petri nets.
-}
module Modelling.PetriNet.Parser (
  convertPetri,
  parseChange, parseConflict, parseConcurrency, parsePetriLike,
  parseRenamedPetriLike, petriLikeToGr,
  simpleNameMap, simpleRename, simpleRenameWith,
  ) where

import qualified Data.Bimap                       as BM (
  fromList, lookup,
  )
import qualified Data.Set                         as Set (
  Set, findMin, foldr, lookupMin, null, size
  )
import qualified Data.Map.Lazy                    as Map (
  empty, findIndex, foldlWithKey, foldrWithKey, insert, lookup,
  )

import Modelling.PetriNet.Alloy         (
  conflictTransition1, conflictTransition2, conflictPlace1,
  concurrencyTransition1, concurrencyTransition2,
  )
import Modelling.PetriNet.Types

import Control.Arrow                    (left, second)
import Language.Alloy.Call
import Data.Bimap                       (Bimap)
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.List                        (stripPrefix)
import Data.Set                         (Set)
import Data.Map                         (Map)
import Data.Maybe                       (fromMaybe)

{-|
Given the name of a flow set and a token set the given alloy instance is parsed
to a 'PetriLike' graph and a 'Petri' is returned if the instance is indeed a
valid Petri net (after applying 'petriLikeToPetri').
-}
convertPetri
  :: String              -- ^ the name of the flow set
  -> String              -- ^ the name of the token set
  -> AlloyInstance       -- ^ the Petri net 'AlloyInstance'
  -> Either String Petri
convertPetri f t inst = do
  p <- parsePetriLike f t inst
  petriLikeToPetri p

{-|
Parse a 'PetriLike' graph from an 'AlloyInstance' given the instances flow and
token set names.
And return an already renamed Petri net.
-}
parseRenamedPetriLike
  :: String
  -> String
  -> AlloyInstance
  -> Either String (PetriLike String)
parseRenamedPetriLike flowSetName tokenSetName inst= do
  petriLike <- parsePetriLike flowSetName tokenSetName inst
  let rename = simpleRenameWith petriLike
  traversePetriLike rename petriLike

{-|
Transform a given value into a 'String' by replacing it according to the
'simpleNameMap' retrieved by the given 'PetriLike'.
-}
simpleRenameWith :: Ord a => PetriLike a -> a -> Either String String
simpleRenameWith petriLike x = do
  let nameMap = simpleNameMap petriLike
  left show $ BM.lookup x nameMap

{-|
Parse a `PetriLike' graph from an 'AlloyInstance' given the instances flow and
token set names.
-}
parsePetriLike
  :: String                           -- ^ the name of the flow set
  -> String                           -- ^ the name of the token set
  -> AlloyInstance                    -- ^ the Petri net 'AlloyInstance'
  -> Either String (PetriLike Object)
parsePetriLike flowSetName tokenSetName inst = do
  nodes  <- singleSig inst "this" "Nodes" ""
  tkns   <- doubleSig inst "this" "Places" tokenSetName
  tokens <- relToMap (second $ read . objectName) tkns
  flow   <- tripleSig inst "this" "Nodes" flowSetName
  fin    <- relToMap tripleToIn flow
  fin'   <- relToMap id `mapM` fin
  fout   <- relToMap tripleToOut flow
  fout'  <- relToMap id `mapM` fout
  return $ PetriLike $
    Set.foldr (\x -> Map.insert x (toNode tokens fin' fout' x)) Map.empty nodes
  where
    tripleToIn  (x, y, z) = (y, (x, read $ objectName z))
    tripleToOut (x, y, z) = (x, (y, read $ objectName z))

{-|
Transform an 'Object' into a 'String' by replacing the prefix.
Returns 'Either':

 * an error message if no matching prefix was found
 * or the resulting 'String'
-}
simpleRename :: Object -> Either String String
simpleRename x
  | Just y <- strip "addedPlaces$"      = Right $ 'a':'S':y
  | Just y <- strip "addedTransitions$" = Right $ 'a':'T':y
  | Just y <- strip "givenPlaces$"      = Right $ 'S':y
  | Just y <- strip "givenTransitions$" = Right $ 'T':y
  | otherwise
  = Left $ "simpleRename: Could not rename " ++ show x
  where
    strip pre = stripPrefix pre $ objectName x

{-|
Given three maps, and a given key construct a node for that key, containing its
initial token (if available), its 'Map's for flow from and to other nodes.
-}
toNode
  :: Ord k
  => Map k (Set Int)
  -- ^ the initial markings (i.e. initial tokens) as a map of singleton sets
  -> Map k (Map k (Set Int))
  -- ^ all flow out
  -> Map k (Map k (Set Int))
  -- ^ all flow in
  -> k
  -> Node k
toNode tokens fin fout x = case Map.lookup x tokens >>= Set.lookupMin of
  Nothing -> TransitionNode {
    flowIn  = toFlow fin,
    flowOut = toFlow fout
    }
  Just t  -> PlaceNode {
    initial = t,
    flowIn  = toFlow fin,
    flowOut = toFlow fout
    }
  where
    toFlow flow = fromMaybe Map.empty $ do
      xs <- Map.lookup x flow
      Set.lookupMin `mapM` xs

{-|
Parses a 'PetriChange' given an 'AlloyInstance'.
On error a 'Left' error message will be returned.
-}
parseChange :: AlloyInstance -> Either String (PetriChange Object)
parseChange inst = do
  flow <- tripleSig inst "this" "Nodes" "flowChange"
  tkn  <- doubleSig inst "this" "Places" "tokenChange"
  tknM   <- relToMap (second $ read . objectName) tkn
  tknC   <- asSingleton `mapM` tknM
  flowM  <- relToMap tripleToOut flow
  flowM' <- relToMap id `mapM` flowM
  flowC  <- mapM asSingleton `mapM` flowM'
  return $ Change {
    tokenChange = tknC,
    flowChange  = flowC
    }
  where
    tripleToOut (x, y, z) = (x, (y, read $ objectName z))

{-|
Parses the conflict Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConflict :: AlloyInstance -> Either String (PetriConflict Object)
parseConflict inst = do
  tc1 <- unscopedSingleSig inst conflictTransition1 ""
  tc2 <- unscopedSingleSig inst conflictTransition2 ""
  pc  <- unscopedSingleSig inst conflictPlace1 ""
  Conflict
    <$> ((,) <$> asSingleton tc1 <*> asSingleton tc2)
    <*> asSingleton pc

{-|
Parses the concurrency Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConcurrency :: AlloyInstance -> Either String (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

{-|
Convert a singleton 'Set' into its single value.
Returns a 'Left' error message if the 'Set' is empty or contains more than one
single element.
-}
asSingleton :: Set b -> Either String b
asSingleton s
  | Set.null s
  = Left "Expected a singleton element but got an empty set"
  | Set.size s /= 1
  = Left "Expected a singleton element but got multiple elements"
  | otherwise
  = Right $ Set.findMin s

singleSig :: AlloyInstance -> String -> String -> String -> Either String (Set.Set Object)
singleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getSingle rd sig
                            
doubleSig :: AlloyInstance -> String -> String -> String -> Either String (Set.Set (Object,Object))
doubleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getDouble rd sig

tripleSig :: AlloyInstance -> String -> String -> String
               -> Either String (Set.Set (Object,Object,Object))
tripleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getTriple rd sig
  
unscopedSingleSig :: AlloyInstance -> String -> String -> Either String (Set.Set Object)
unscopedSingleSig inst st nd = do
  sig <- lookupSig (unscoped st) inst
  getSingle nd sig

{-|
Retrieve a simple naming map from a given 'PetriLike'.
The newly created names for naming every 'Node' of the 'PetriLike' are unique
for each individually 'Node'.
Furthermore, each 'PlaceNode's names prefix is a @s@, while each
'TransitionNode's name is preceded by a @t@.
These prefixes are followed by numbers starting by 1 and reaching to the number
of 'PlaceNode's and 'TransitionNode's respectively.
-}
simpleNameMap :: Ord a => PetriLike a -> Bimap a String
simpleNameMap pl = BM.fromList . fst <$>
  Map.foldlWithKey
  nameIncreasingly
  ([], (1 :: Integer, 1 :: Integer))
  $ allNodes pl
  where
    nameIncreasingly (ys, (p, t)) k x =
      let (k', p', t') = step x p t
      in ((k, k'):ys, (p', t'))
    step n p t = case n of
      PlaceNode {}      -> ('s':show p, p + 1, t)
      TransitionNode {} -> ('t':show t, p, t + 1)

{-|
Convert a 'PetriLike' into a 'Gr' enabling to draw it using graphviz.
-}
petriLikeToGr
  :: Ord a
  => PetriLike a
  -> Either a (Gr (a, Maybe Int) Int)
petriLikeToGr plike = do
  nodes <- Map.foldrWithKey convertNode (return []) $ allNodes plike
  let edges = Map.foldrWithKey convertTransition [] $ allNodes plike
  return $ mkGraph nodes edges
  where
    convertNode k x ns = do
      ns' <- ns
      return $ (indexOf k, (k, maybeInitial x)):ns'
    convertTransition k x ns =
      Map.foldrWithKey (convertEdge k) ns $ flowIn x
    indexOf x = Map.findIndex x $ allNodes plike
    convertEdge k source target rs =
      (indexOf source, indexOf k, target) : rs
