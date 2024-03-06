{-|
A module for parsing Petri Alloy instances into Haskell representations defined
by the 'Modelling.PetriNet.Types' module.
The instances contain valid and invalid Petri nets that is why these are parsed
into types as 'Net' which allow representing some invalid representations
of graphs which are similar to Petri nets.
-}
module Modelling.PetriNet.Parser (
  asSingleton,
  convertPetri,
  netToGr,
  parseChange,
  parseNet,
  parseRenamedNet,
  simpleNameMap, simpleRename, simpleRenameWith,
  ) where

import qualified Modelling.PetriNet.Types         as PN (
  Net (nodes),
  )

import qualified Data.Bimap                       as BM (
  fromList, lookup,
  )
import qualified Data.Set                         as Set (
  Set, findMin, fromList, lookupMin, null, size, toList,
  )
import qualified Data.Map.Lazy                    as Map (
  findIndex,
  foldlWithKey',
  foldrWithKey,
  lookup,
  )

import Modelling.Auxiliary.Common       (Object (Object, oName, oIndex), toMap)
import Modelling.PetriNet.Types (
  Net (emptyNet, outFlow, alterFlow, alterNode, traverseNet),
  Petri,
  PetriChange (..),
  PetriNode (..),
  maybeInitial,
  petriLikeToPetri,
  )

import Control.Arrow                    (left, second)
import Data.Bimap                       (Bimap)
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.Set                         (Set)
import Data.Map                         (Map)
import Data.Composition                 ((.:))
import Language.Alloy.Call (
  AlloyInstance,
  getDoubleAs,
  getSingleAs,
  getTripleAs,
  lookupSig,
  scoped,
  )

{-|
Given the name of a flow set and a token set the given alloy instance is parsed
to a 'Net' graph and a 'Petri' is returned if the instance is indeed a
valid Petri net (after applying 'petriLikeToPetri').
-}
convertPetri
  :: String              -- ^ the name of the flow set
  -> String              -- ^ the name of the token set
  -> AlloyInstance       -- ^ the Petri net 'AlloyInstance'
  -> Either String Petri
convertPetri f t inst = do
  p <- parseNet f t inst
  petriLikeToPetri p

{-|
Parse a 'Net' graph from an 'AlloyInstance' given the instances flow and
token set names.
And return an already renamed Petri net.
-}
parseRenamedNet
  :: Net p n
  => String
  -> String
  -> AlloyInstance
  -> Either String (p n String)
parseRenamedNet flowSetName tokenSetName inst = do
  petriLike <- parseNet flowSetName tokenSetName inst
  let rename = simpleRenameWith petriLike
  traverseNet rename petriLike

{-|
Transform a given value into a 'String' by replacing it according to the
'simpleNameMap' retrieved by the given 'Net'.
-}
simpleRenameWith :: (Net p n, Ord a) => p n a -> a -> Either String String
simpleRenameWith petriLike x = do
  let nameMap = simpleNameMap petriLike
  left show $ BM.lookup x nameMap

{-|
Parse a `Net' graph from an 'AlloyInstance' given the instances flow and
token set names.
-}
parseNet
  :: Net p n
  => String                           -- ^ the name of the flow set
  -> String                           -- ^ the name of the token set
  -> AlloyInstance                    -- ^ the Petri net 'AlloyInstance'
  -> Either String (p n Object)
parseNet flowSetName tokenSetName inst = do
  nodes  <- singleSig inst "this" "Nodes" ""
  tkns   <- doubleSig inst "this" "Places" tokenSetName
  let tokens = relToMap (second oIndex) tkns
  flow   <- tripleSig inst "this" "Nodes" flowSetName
  return
    . foldrFlip (\(x, y, z) -> alterFlow x (oIndex z) y) flow
    . foldrFlip
      (\x -> alterNode x $ Map.lookup x tokens >>= Set.lookupMin)
      nodes
    $ emptyNet
  where
    foldrFlip f = flip $ foldr f

relToMap :: (Ord b, Ord c) => (a -> (b, c)) -> Set a -> Map b (Set c)
relToMap f = toMap . Set.fromList . map f . Set.toList

{-|
Transform an 'Object' into a 'String' by replacing the prefix.
Returns 'Either':

 * an error message if no matching prefix was found
 * or the resulting 'String'
-}
simpleRename :: Object -> Either String String
simpleRename x = case oName x of
  "addedPlaces"      -> Right $ 'a':'S':y
  "addedTransitions" -> Right $ 'a':'T':y
  "givenPlaces"      -> Right $ 'S':y
  "givenTransitions" -> Right $ 'T':y
  _                  ->
    Left $ "simpleRename: Could not rename " ++ oName x ++ '$' : y
  where
    y = show (oIndex x)

{-|
Parses a 'PetriChange' given an 'AlloyInstance'.
On error a 'Left' error message will be returned.
-}
parseChange :: AlloyInstance -> Either String (PetriChange Object)
parseChange inst = do
  flow <- tripleSig inst "this" "Nodes" "flowChange"
  tkn  <- doubleSig inst "this" "Places" "tokenChange"
  let tknM = relToMap (second oIndex) tkn
  tknC   <- asSingleton `mapM` tknM
  let flowM = relToMap tripleToOut flow
  let flowM' = relToMap id <$> flowM
  flowC  <- mapM asSingleton `mapM` flowM'
  return $ Change {
    tokenChange = tknC,
    flowChange  = flowC
    }
  where
    tripleToOut (x, y, z) = (x, (y, oIndex z))

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
  getSingleAs rd (return .: Object) sig

doubleSig :: AlloyInstance -> String -> String -> String -> Either String (Set.Set (Object,Object))
doubleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  let obj = return .: Object
  getDoubleAs rd obj obj sig

tripleSig :: AlloyInstance -> String -> String -> String
               -> Either String (Set.Set (Object,Object,Object))
tripleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  let obj = return .: Object
  getTripleAs rd obj obj obj sig

{-|
Retrieve a simple naming map from a given 'Net'.
The newly created names for naming every 'PetriNode' of the 'Net' are unique
for each individually 'PetriNode'.
Furthermore, each place node's names prefix is a @s@, while each
transition node's name is preceded by a @t@.
These prefixes are followed by numbers starting at 1 and reaching to the number
of place nodes and transition nodes respectively.
-}
simpleNameMap :: (Net p n, Ord a) => p n a -> Bimap a String
simpleNameMap pl = BM.fromList . fst <$>
  Map.foldlWithKey'
  nameIncreasingly
  ([], (1 :: Integer, 1 :: Integer))
  $ PN.nodes pl
  where
    nameIncreasingly (ys, (p, t)) k x =
      let (k', p', t') = step x p t
      in ((k, k'):ys, (p', t'))
    step n p t
      | isPlaceNode n = ('s':show p, p + 1, t)
      | otherwise     = ('t':show t, p, t + 1)

{-|
Convert a 'Net' into a 'Gr' enabling to draw it using graphviz.
-}
netToGr
  :: (Net p n, Ord a)
  => p n a
  -> Either a (Gr (a, Maybe Int) Int)
netToGr plike = do
  nodes <- Map.foldrWithKey convertNode (return []) $ PN.nodes plike
  let edges = Map.foldrWithKey convertTransition [] $ PN.nodes plike
  return $ mkGraph nodes edges
  where
    convertNode k x ns = do
      ns' <- ns
      return $ (indexOf k, (k, maybeInitial x)):ns'
    convertTransition k _ ns =
      Map.foldrWithKey (convertEdge k) ns $ outFlow k plike
    indexOf x = Map.findIndex x $ PN.nodes plike
    convertEdge source target flow rs =
      (indexOf source, indexOf target, flow) : rs
