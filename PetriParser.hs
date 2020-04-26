--{-# LANGUAGE NamedFieldPuns #-}

module PetriParser where

import Data.List
import Data.Maybe
import Data.String
import Language.Alloy.Call
import qualified Data.Set as Set
import AuxFunctions
import Types
import PetriAlloy
      
type TripSet = Set.Set (Object,Object,Object)

convertPetri :: AlloyInstance -> IO(Either String Petri)
convertPetri inst = do
  flow <- filterFlow inst
  case flow of
    Left error -> return $ Left error
    Right pFlow -> do
      mark <- startMark inst
      case mark of
        Left error -> return $ Left error
        Right pMark -> return $ Right (Petri{startM = pMark,trans = pFlow})
      
                          --Transitionen--

--[([(3)],[(3)])] List of TransSets
filterFlow :: AlloyInstance -> IO(Either String [Trans])
filterFlow inst = do
  let trn = singleSig inst "this" "Transitions" ""
  case trn of
    Left error -> return $ Left error
    Right trans -> do
      let out = tripleSig inst 
      case out of 
        Left error -> return $ Left error
        Right set -> do
          let flow = filterTrans (Set.toList trans) set
          let plcs = singleSig inst "this" "Places" ""
          case plcs of 
            Left error -> return $ Left error
            Right places -> return $ Right $ convertToTrans (Set.toList places) flow
            
      

filterTrans :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTrans [] _ = []
filterTrans (t:rs) set = (filterSndTrip t set,filterFstTrip t set) : filterTrans rs set

convertToTrans :: [Object] -> [(TripSet,TripSet)] -> [Trans]
convertToTrans _ [] = []
convertToTrans ls ((a,b):rs) = (helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b)) 
                                : convertToTrans ls rs 



                         --Startmarkierung--
startMark :: AlloyInstance -> IO (Either String Mark)
startMark inst =
  case doubleSig inst "this" "Places" "tokens" of
    Left error -> return $ Left error
    Right smark -> return $ Right $ convertTuple (Set.toList smark)

      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = (read (objectName i) :: Int) : convertTuple rs

  

                            --Hilfsfunktionen--
-- validConfig :: Input -> Bool
-- validConfig Input{places,transitions,tkns,maxTkns,maxWght,activated} = 
  -- places > 0 && transitions > 0 && tkns > 0 &&
  -- maxTkns <= tkns &&
  -- tkns <= places*maxTkns &&
  -- activated <= transitions &&
  -- maxWght <= maxTkns 
                            
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
  where helpFilter a (x,_,_) = a == x
  
filterSndTrip :: Object -> Set.Set (Object,Object,Object) -> Set.Set (Object,Object,Object)
filterSndTrip a = Set.filter $ helpFilter a
  where helpFilter a (_,x,_) = a == x
  
helpConvertPre :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPre [] _ = []
helpConvertPre (p:rp) [] = 0: helpConvertPre rp []
helpConvertPre (p:rp) list@((a,b,x):rt)
 | p == a = (read (objectName x) :: Int) : helpConvertPre rp rt
 | otherwise = 0 : helpConvertPre rp list
 
helpConvertPost :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPost [] _ = []
helpConvertPost (p:rp) [] = 0: helpConvertPost rp []
helpConvertPost (p:rp) list@((a,b,x):rt)
 | p == b = (read (objectName x) :: Int) : helpConvertPost rp rt
 | otherwise = 0 : helpConvertPost rp list
----------------------------------------Main(-s)--------------------------------------------------
  
runPParser :: Input -> IO(Either String Petri)
runPParser inp = do 
    list <- getInstances (Just 5) (petriNetRnd inp)
    convertPetri(head list)

    
    
    
  

----------------------------------------------------------------------
----------------------------------------------------------------------