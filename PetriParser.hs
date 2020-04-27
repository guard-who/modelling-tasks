--{-# LANGUAGE NamedFieldPuns #-}

module PetriParser where

import Language.Alloy.Call
import qualified Data.Set as Set
import Types
import PetriAlloy
      
type TripSet = Set.Set (Object,Object,Object)

convertPetri :: AlloyInstance -> Either String Petri
convertPetri inst = do
  flow <- filterFlow inst
  mark <- startMark inst
  return $ Petri{startM = mark,trans = flow}
      
                          --Transitionen--

--[([(3)],[(3)])] List of TransSets
filterFlow :: AlloyInstance -> Either String [Trans]
filterFlow inst = do
  trns <- singleSig inst "this" "Transitions" ""
  set  <- tripleSig inst
  let flow = filterTrans (Set.toList trns) set
  plcs <- singleSig inst "this" "Places" ""
  return $ convertToTrans (Set.toList plcs) flow

filterTrans :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTrans [] _ = []
filterTrans (t:rs) set = (filterSndTrip t set,filterFstTrip t set) : filterTrans rs set

convertToTrans :: [Object] -> [(TripSet,TripSet)] -> [Trans]
convertToTrans _ [] = []
convertToTrans ls ((a,b):rs) = (helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b)) 
                                : convertToTrans ls rs 



                         --Startmarkierung--
startMark :: AlloyInstance -> Either String Mark
startMark inst = do
  mark <- doubleSig inst "this" "Places" "tokens"
  return $ convertTuple (Set.toList mark)

      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = (read (objectName i) :: Int) : convertTuple rs

  

                            --Hilfsfunktionen--
                            
-- Instance -> scoped? -> relations (e.g. ["this","Nodes","flow"])
singleSig :: AlloyInstance -> String -> String -> String -> Either String (Set.Set Object)
singleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getSingle rd sig
                            
doubleSig :: AlloyInstance -> String -> String -> String -> Either String (Set.Set (Object,Object))
doubleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getDouble rd sig

tripleSig :: AlloyInstance -> Either String (Set.Set (Object,Object,Object))
tripleSig inst = do
  sig <- lookupSig (scoped "this" "Nodes") inst
  getTriple "flow" sig
  
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
----------------------------------------Main(-s)--------------------------------------------------
  
runPParser :: Input -> IO(Either String Petri)
runPParser inp = do 
    list <- getInstances (Just 5) (petriNetRnd inp)
    return $ convertPetri(head list)

    
    
    
  

----------------------------------------------------------------------
----------------------------------------------------------------------