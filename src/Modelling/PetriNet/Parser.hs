{-# LANGUAGE ParallelListComp #-}

module Modelling.PetriNet.Parser (convertPetri, convertGr, prepNodes, parseChange, parseConflict, parseConcurrency) where

import Modelling.PetriNet.Types

import Language.Alloy.Call
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import qualified Data.Map.Lazy as Map
import Data.Maybe                       (fromMaybe)
      
type TripSet = Set.Set (Object,Object,Object)

convertPetri :: String -> String -> AlloyInstance -> Either String Petri
convertPetri f t inst = do
  flow <- filterFlow f inst
  mark <- startMark t inst
  return $ Petri{initialMarking = mark,trans = flow}
      
                          --Transitionen--

--[([(3)],[(3)])] List of TransSets
filterFlow :: String -> AlloyInstance -> Either String [Transition]
filterFlow f inst = do
  trns <- singleSig inst "this" "Transitions" ""
  set  <- tripleSig inst "this" "Nodes" f
  let flow = filterTransitions (Set.toList trns) set
  plcs <- singleSig inst "this" "Places" ""
  return $ convertToTransitions (Set.toList plcs) flow

-- prepare with given Transitions the Pre and Post Sets for Petri
filterTransitions :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTransitions [] _ = []
filterTransitions (t:rs) set = (filterSndTrip t set,filterFstTrip t set) : filterTransitions rs set

-- make out of the given Sets the Transitions for Petri
convertToTransitions :: [Object] -> [(TripSet,TripSet)] -> [Transition]
convertToTransitions _ [] = []
convertToTransitions ls ((a,b):rs) = (helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b))
                                : convertToTransitions ls rs


                         --Startmarkierung--
startMark :: String -> AlloyInstance -> Either String Marking
startMark s inst = do
  mark <- doubleSig inst "this" "Places" s
  return $ convertTuple (Set.toList mark)

      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = (read (objectName i) :: Int) : convertTuple rs

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
  
 
                            
-- Instance -> scope -> relations (e.g. ["this","Nodes","flow"])
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
  
                      --Filter for Objects--
filterFstTrip :: Object -> Set.Set (Object,Object,Object) -> Set.Set (Object,Object,Object)
filterFstTrip a = Set.filter $ helpFilter a
  where helpFilter b (x,_,_) = b == x
  
filterSndTrip :: Object -> Set.Set (Object,Object,Object) -> Set.Set (Object,Object,Object)
filterSndTrip a = Set.filter $ helpFilter a
  where helpFilter b (_,x,_) = b == x
  
helpConvertPre :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPre [] _ = []
helpConvertPre (_:rp) [] = 0: helpConvertPre rp []
helpConvertPre (p:rp) list@((a,_,x):rt)
 | p == a = (read (objectName x) :: Int) : helpConvertPre rp rt
 | otherwise = 0 : helpConvertPre rp list
 
helpConvertPost :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPost [] _ = []
helpConvertPost (_:rp) [] = 0: helpConvertPost rp []
helpConvertPost (p:rp) list@((_,b,x):rt)
 | p == b = (read (objectName x) :: Int) : helpConvertPost rp rt
 | otherwise = 0 : helpConvertPost rp list
 
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

------------------------------------------------------------------------------------------
--Parse From Input
-- runIParser :: Input -> IO(Either String Petri)
-- runIParser inp = do 
    -- list <- getInstances (Just 5) (petriNetRnd inp)
    -- return $ convertPetri "tokens" (head list)
