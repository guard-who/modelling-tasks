
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
  let trn = singleSig inst ["this","Transitions",""]
  case trn of
    Left error -> return $ Left error
    Right trans -> do
      let out = tripleSig inst 
      case out of 
        Left error -> return $ Left error
        Right set -> do
          let flow = filterTrans (Set.toList trans) set
          let plcs = singleSig inst ["this","Places",""]
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
  case doubleSig inst ["this","Places","tokens"] of
    Left error -> return $ Left error
    Right smark -> return $ Right $ convertTuple (Set.toList smark)

      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = ((read (objectName i) :: Int) : (convertTuple rs))

  

                            --Hilfsfunktionen--
                           
                            
-- Instance -> scoped? -> relations (e.g. ["this","Nodes","flow"])
singleSig :: AlloyInstance -> [String] -> Either String (Set.Set Object)
singleSig inst lst = do
  sig <- lookupSig (scoped (head lst) (head (tail lst))) inst
  getSingle (lst !! 2) sig
                            
doubleSig :: AlloyInstance -> [String] -> Either String (Set.Set (Object,Object))
doubleSig inst lst = do
  sig <- lookupSig (scoped (head lst) (head (tail lst))) inst
  getDouble (lst !! 2) sig

tripleSig :: AlloyInstance -> Either String (Set.Set (Object,Object,Object))
tripleSig inst = do
  sig <- lookupSig (scoped "this" "Nodes") inst
  getTriple "flow" sig
  
                      --Filter for Objects--
filterFstTrip :: Object -> (Set.Set (Object,Object,Object)) -> (Set.Set (Object,Object,Object))
filterFstTrip a set = Set.filter (\s -> helpFilter a s) set
  where helpFilter a (x,_,_) = a == x
  
filterSndTrip :: Object -> (Set.Set (Object,Object,Object)) -> (Set.Set (Object,Object,Object))
filterSndTrip a set = Set.filter (\s -> helpFilter a s) set
  where helpFilter a (_,x,_) = a == x
  
helpConvertPre :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPre [] _ = []
helpConvertPre (p:rp) [] = (0: helpConvertPre rp [])
helpConvertPre (p:rp) ((a,b,x):rt)
 | p == a = ((read (objectName x) :: Int) : helpConvertPre rp rt)
 | otherwise = (0 : helpConvertPre rp ((a,b,x):rt) )
 
helpConvertPost :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPost [] _ = []
helpConvertPost (p:rp) [] = (0: helpConvertPost rp [])
helpConvertPost (p:rp) ((a,b,x):rt)
 | p == b = ((read (objectName x) :: Int) : helpConvertPost rp rt)
 | otherwise = (0 : helpConvertPost rp ((a,b,x):rt) )
----------------------------------------Testing--------------------------------------------------

testPParser :: IO()
testPParser = do
  let inp = defaultInput{ places = 4, maxWght = 1}
  let scp = petriScope inp
  list <- getInstances (Just 5) (petriNetRnd inp scp )
  petri <- convertPetri (head list)
  print petri

----------------------------------------------------------------------
----------------------------------------------------------------------