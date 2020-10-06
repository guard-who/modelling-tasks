{-# LANGUAGE ParallelListComp #-}

module Modelling.PetriNet.Parser (
  convertPetri, convertGr, prepNodes,
  parseChange, parseConflict, parseConcurrency, parsePetriLike,
  simpleNameMap, simpleRename, petriLikeToGr
  ) where

import qualified Data.Bimap                       as BM (
  fromList, lookup,
  )
import qualified Data.Set                         as Set (
  Set, findMin, foldr, lookupMin, null, size, toList
  )
import qualified Data.Map.Lazy                    as Map (
  Map, empty, findIndex, foldlWithKey, foldrWithKey, fromList, insert, lookup,
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
Parse a `PetriLike' graph from an 'AlloyInstance' given the instances flow and
token sets.
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

                          --get Abweichung--
parseChange :: AlloyInstance -> Either String Change 
parseChange inst = do
  flow <- tripleSig inst "this" "Nodes" "flowChange"
  tkn  <- doubleSig inst "this" "Places" "tokenChange"
  let flowC = flowChangeP (Set.toList flow)
  let tknC  = tokenChangeP (Set.toList tkn)
  return $ Change{tokenChange = tknC , flowChange = flowC}

flowChangeP :: [(Object,Object,Object)] -> [(String,String,Int)]
flowChangeP []               = []
flowChangeP ((n1,n2,val):rs) =
  (listTill (objectName n1) '$' ,listTill (objectName n2) '$', read (objectName val) :: Int)
  : flowChangeP rs
  
tokenChangeP :: [(Object,Object)] -> [(String,Int)]
tokenChangeP []            = []
tokenChangeP ((pl,val):rt) = (listTill (objectName pl) '$' , read (objectName val) :: Int) 
                             : tokenChangeP rt

                            --Spezielles--                
{-|
Parses the conflict skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConflict :: AlloyInstance -> Either String (Conflict Object)
parseConflict inst = do
  tc1 <- unscopedSingleSig inst conflictTransition1 ""
  tc2 <- unscopedSingleSig inst conflictTransition2 ""
  pc  <- unscopedSingleSig inst conflictPlace1 ""
  Conflict
    <$> ((,) <$> asSingleton tc1 <*> asSingleton tc2)
    <*> asSingleton pc

{-|
Parses the concurrency skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConcurrency :: AlloyInstance -> Either String (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  (,) <$> asSingleton t1 <*> asSingleton t2

asSingleton :: Set b -> Either String b
asSingleton s
  | Set.null s
  = Left "Expected a singleton element but got an empty set"
  | Set.size s /= 1
  = Left "Expected a singleton element but got multiple elements"
  | otherwise
  = Right $ Set.findMin s

                            --Hilfsfunktionen--                                
mapNodes :: AlloyInstance -> Either String (Map.Map Object Int)
mapNodes inst = do
  nods <- singleSig inst "this" "Nodes" ""
  return $ Map.fromList $ Set.toList nods `zip`  [0..]

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
  
--getList up to given element
listTill :: (Eq a) => [a] -> a -> [a]
listTill [] _ = []
listTill (x:rs) y 
 | x == y    = []
 | otherwise = x : listTill rs y
 
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
      PlaceNode {}      -> ('S':show p, p + 1, t)
      TransitionNode {} -> ('T':show t, p, t + 1)

petriLikeToGr
  :: Ord a
  => PetriLike a
  -> Bimap a String
  -> Either String (Gr (String, Maybe Int) String)
petriLikeToGr plike names = left show $ do
  nodes <- Map.foldrWithKey convertNode (return []) $ allNodes plike
  let edges = Map.foldrWithKey convertTransition [] $ allNodes plike
  return $ mkGraph nodes edges
  where
    convertNode k x ns = do
      ns' <- ns
      k'  <- BM.lookup k names
      return $ (indexOf k, (k', maybeInitial x)):ns'
    convertTransition k x ns =
      Map.foldrWithKey (convertEdge k) ns $ flowIn x
    indexOf x = Map.findIndex x $ allNodes plike
    convertEdge k source target rs =
      (indexOf source, indexOf k, show target) : rs
------------------------------------------------------------------------------------------
--ParseDirectlyToDiagram

convertGr :: String -> [(Int,(String, Maybe Int))] -> AlloyInstance -> Either String (Gr(String,Maybe Int) String)
convertGr f n inst = do
  flow <- tripleSig inst "this" "Nodes" f
  nodesM <- mapNodes inst 
  return $ mkGraph n 
    [(getVal pr nodesM, getVal po nodesM, objectName wg)| (pr,po,wg) <- Set.toList flow]
  
prepNodes :: String -> AlloyInstance -> Either String [(Int,(String, Maybe Int))]
prepNodes t inst = do
  pls <- singleSig inst "this" "Places" ""
  trns <- singleSig inst "this" "Transitions" ""
  mark <- doubleSig inst "this" "Places" t
  let mVal = Map.fromList ( Set.toList mark)
  nodes <- mapNodes inst 
  return 
    (  [ (getVal p nodes,("S"++show (i :: Int),Just (read (objectName (getVal p mVal)) :: Int)))
       | p <- Set.toList pls | i <- [1..]]
    ++ [ (getVal tr nodes,("T"++show (i :: Int),Nothing)) 
       | tr <- Set.toList trns | i <- [1..] ]
    )
  
getVal :: Ord k => k -> Map.Map k v -> v
getVal x m = do
  let item = Map.lookup x m
  fromMaybe (error "Error occurred while mapping the net") item
