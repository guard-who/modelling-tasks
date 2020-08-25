{-# LANGUAGE ParallelListComp #-}

module Modelling.PetriNet.Parser (
  convertPetri, convertGr, prepNodes,
  parseChange, parseConflict, parseConcurrency, parsePetriLike,
  simpleRename
  ) where

import qualified Data.Set                         as Set (
  Set, elemAt, foldr, lookupMin, toList
  )
import qualified Data.Map.Lazy                    as Map (
  Map, empty, fromList, insert, lookup
  )

import Modelling.PetriNet.Types

import Control.Arrow                    (second)
import Language.Alloy.Call
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
parseConflict :: [(Int,(String, Maybe Int))] -> AlloyInstance -> Either String Conflict
parseConflict nodes inst = do
  mId <- mapNodes inst
  tc1 <- unscopedSingleSig inst "$showRelNets_conflictTrans1" ""
  tc2 <- unscopedSingleSig inst "$showRelNets_conflictTrans2" ""
  pc  <- unscopedSingleSig inst "$showRelNets_conflictPlace"  ""
  return 
    Conflict
      {conflictTrans = ( extractName (getVal (Set.elemAt 0 tc1) mId) nodes
                       , extractName (getVal (Set.elemAt 0 tc2) mId) nodes
                       )
      ,conflictPlace = extractName (getVal (Set.elemAt 0 pc) mId) nodes
      }
    
parseConcurrency :: [(Int,(String, Maybe Int))] -> AlloyInstance -> Either String Concurrent
parseConcurrency nodes inst = do
  mId <- mapNodes inst
  tc1 <- unscopedSingleSig inst "$showRelNets_concurTrans1" ""
  tc2 <- unscopedSingleSig inst "$showRelNets_concurTrans2" ""
  return
    ( extractName (getVal (Set.elemAt 0 tc1) mId) nodes
    , extractName (getVal (Set.elemAt 0 tc2) mId) nodes
    )
                            
                            --Hilfsfunktionen--                                
mapNodes :: AlloyInstance -> Either String (Map.Map Object Int)
mapNodes inst = do
  nods <- singleSig inst "this" "Nodes" ""
  return $ Map.fromList $ Set.toList nods `zip`  [0..]

extractName :: Int -> [(Int,(String, Maybe Int))] -> String
extractName i nodes =
  fst $ fromMaybe (error "Error occurred while mapping the net") (lookup i nodes)
  
 
                            

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
