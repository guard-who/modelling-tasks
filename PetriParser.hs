{-# LANGUAGE NamedFieldPuns #-}

module PetriParser where

import Language.Alloy.Call
import qualified Data.Set as Set
import Types
import PetriAlloy
      
type TripSet = Set.Set (Object,Object,Object)

convertPetri :: String -> AlloyInstance -> Either String Petri
convertPetri s inst = do
  flow <- filterFlow inst
  mark <- startMark s inst
  return $ Petri{startM = mark,trans = flow}
      
                          --Transitionen--

--[([(3)],[(3)])] List of TransSets
filterFlow :: AlloyInstance -> Either String [Trans]
filterFlow inst = do
  trns <- singleSig inst "this" "Transitions" ""
  set  <- tripleSig inst "this" "Nodes" "flow"
  let flow = filterTrans (Set.toList trns) set
  plcs <- singleSig inst "this" "Places" ""
  return $ convertToTrans (Set.toList plcs) flow

-- prepare with given Transitions the Pre and Post Sets for Petri
filterTrans :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTrans [] _ = []
filterTrans (t:rs) set = (filterSndTrip t set,filterFstTrip t set) : filterTrans rs set

-- make out of the given Sets the Transitions for Petri
convertToTrans :: [Object] -> [(TripSet,TripSet)] -> [Trans]
convertToTrans _ [] = []
convertToTrans ls ((a,b):rs) = (helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b)) 
                                : convertToTrans ls rs 



                         --Startmarkierung--
startMark :: String -> AlloyInstance -> Either String Mark
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
  let flowC = flowChange (Set.toList flow)
  let tknC  = tokenChange (Set.toList tkn)
  return $ Change{tknChange = tknC , flwChange = flowC}

flowChange :: [(Object,Object,Object)] -> [(String,String,Int)]
flowChange []               = []
flowChange ((n1,n2,val):rs) =
  (listTill (objectName n1) '$' ,listTill (objectName n2) '$',(read (objectName val) :: Int))
  : flowChange rs
  
tokenChange :: [(Object,Object)] -> [(String,Int)]
tokenChange []            = []
tokenChange ((pl,val):rt) = (listTill (objectName pl) '$' ,(read (objectName val) :: Int)) 
                             : tokenChange rt
  

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

tripleSig :: AlloyInstance -> String -> String -> String
               -> Either String (Set.Set (Object,Object,Object))
tripleSig inst st nd rd = do
  sig <- lookupSig (scoped st nd) inst
  getTriple rd sig
  
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
----------------------------------------Main(-s)--------------------------------------------------

--Parse From Input
runIParser :: Input -> IO(Either String Petri)
runIParser inp = do 
    list <- getInstances (Just 5) (petriNetRnd inp)
    return $ convertPetri "tokens" (head list)
    
--Parse From String(Alloy Code)
runAParser :: AlloyInstance -> IO(Either String Petri,Either String Change)
runAParser alloy = do
  let petri = convertPetri "tokens" alloy
  let change = parseChange alloy
  return (petri,change)
  

  

----------------------------------------------------------------------
----------------------------------------------------------------------