{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module PetriParser where

import Data.List
import Data.Maybe
import Data.String
import Data.String.Interpolate
import Data.FileEmbed
import Language.Alloy.Call
import qualified Data.Set as Set
import AuxFunctions
      
type TripSet = Set.Set (Object,Object,Object)

data Input = Input
  { places :: Int
  , transitions :: Int
  , tkns :: Int
  , maxTkns :: Int
  , maxWght :: Int
  , activated :: Int
  , petriScope :: Int
  , anyOtherFieldThatMightBeNeededLater :: Bool
  }

defaultInput :: Input
defaultInput = Input
  { places = 3
  , transitions = 3
  , tkns = 4
  , maxTkns = 2
  , maxWght = 2
  , activated = 1
  , petriScope = 10
  , anyOtherFieldThatMightBeNeededLater = undefined -- Note how this field is not even mentioned anywhere below.
  }
  

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
  let trn = singleSig inst (Set.fromAscList ["this","Transitions",""])
  case trn of
    Left error -> return $ Left error
    Right trans -> do
      let out = tripleSig inst 
      case out of 
        Left error -> return $ Left error
        Right set -> do
          let flow = filterTrans (Set.toList trans) set
          let plcs = singleSig inst (Set.fromAscList ["this","Places",""])
          case plcs of 
            Left error -> return $ Left error
            Right places -> do
              return $ Right$ convertToTrans (Set.toList places) flow
            
      

filterTrans :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTrans [] _ = []
filterTrans (t:rs) set = ((filterSndTrip t set,filterFstTrip t set) : (filterTrans rs set))

convertToTrans :: [Object] -> [(TripSet,TripSet)] -> [Trans]
convertToTrans _ [] = []
convertToTrans ls ((a,b):rs) = ((helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b)) 
                                : convertToTrans ls rs )



                         --Startmarkierung--
startMark :: AlloyInstance -> IO (Either String Mark)
startMark inst = do
  case doubleSig inst (Set.fromAscList ["this","Places","tokens"]) of
    Left error -> return $ Left error
    Right smark -> return $ Right $ convertTuple (Set.toList smark)

      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = ((read (objectName i) :: Int) : (convertTuple rs))

  

                            --Hilfsfunktionen--
                            
-- Instance -> scoped? -> relations (e.g. ["this","Nodes","flow"])
singleSig :: AlloyInstance -> Set.Set String -> Either String (Set.Set Object)
singleSig inst set = do
  sig <- lookupSig (scoped (Set.elemAt 0 set) (Set.elemAt 1 set)) inst
  getSingle (Set.elemAt 2 set) sig
                            
doubleSig :: AlloyInstance -> Set.Set String -> Either String (Set.Set (Object,Object))
doubleSig inst set = do
  sig <- lookupSig (scoped (Set.elemAt 0 set) (Set.elemAt 1 set)) inst
  getDouble (Set.elemAt 2 set) sig

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
  list <- getInstances (Just 5) (petriNetRnd defaultInput{ places = 4, maxWght = 1} )
  petri <- convertPetri (head list)
  print petri

----------------------------------------------------------------------
----------------------------------------------------------------------

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "libAlloy/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "libAlloy/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "libAlloy/Helpers.als")

modulePetriConcepts :: String 
modulePetriConcepts = removeLines 5 $(embedStringFile "libAlloy/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 4 $(embedStringFile "libAlloy/PetriConstraints.als")

moduleOneLiners :: String 
moduleOneLiners = removeLines 4 $(embedStringFile "libAlloy/OneLiners.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

--Bigger Net needs bigger "run for x"
petriNetRnd :: Input -> String
petriNetRnd Input{places,transitions,tkns,maxTkns,maxWght,activated,petriScope} = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}
#{moduleOneLiners}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [ts : set Transitions, n : Int] {
  #Places = #{places}
  tokensAddedOverall[#{tkns}]
  perPlaceTokensAddedAtMost[#{maxTkns}]
  #Transitions = #{transitions}
  maxWeight[#{maxWght}]
  n >= #{activated}
  numberActivatedTransition[n,ts]
}
run showNets for #{petriScope}

|]

petriNetA :: String
petriNetA = [i|module scenarios/examples/PetriNetA 
#{modulePetriSignature}
//default Petri net

one sig S1 extends givenPlaces{}
one sig S2 extends givenPlaces{}
one sig S3 extends givenPlaces{}
one sig T1 extends givenTransitions{}
one sig T2 extends givenTransitions{}
one sig T3 extends givenTransitions{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

  S1.defaultFlow[T1] = 1
  S1.defaultFlow[T2] = 1
  S1.defaultFlow[T3] = 1

  S2.defaultFlow[T2] = 1
  no S2.defaultFlow[Transitions - T2]

  S3.defaultFlow[T2] = 1
  no S3.defaultFlow[Transitions - T2]

  T1.defaultFlow[S2] = 1
  no T1.defaultFlow[Places - S2]

  no T2.defaultFlow[Places]

  T3.defaultFlow[S3] = 1
  no T3.defaultFlow[Places - S3]

}
|]