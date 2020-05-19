module Modelling.PetriNet.Parser (convertPetri, parseChange, parseConflict) where

import Modelling.PetriNet.Types

import Language.Alloy.Call
import qualified Data.Set as Set
      
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
                            
parseConflict :: AlloyInstance -> Either String Conflict
parseConflict inst = do
  tc1 <- unscopedSingleSig inst "$showConflNets_conflictTrans1" ""
  tc2 <- unscopedSingleSig inst "$showConflNets_conflictTrans2" ""
  pc  <- unscopedSingleSig inst "$showConflNets_conflictPlace"  ""
  return 
    Conflict{conflictTrans = (objectName (Set.elemAt 0 tc1),objectName (Set.elemAt 0 tc2)),conflictPlace = objectName (Set.elemAt 0 pc)}
                            
                            --Hilfsfunktionen--
                            
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

--Parse From Input
-- runIParser :: Input -> IO(Either String Petri)
-- runIParser inp = do 
    -- list <- getInstances (Just 5) (petriNetRnd inp)
    -- return $ convertPetri "tokens" (head list)
