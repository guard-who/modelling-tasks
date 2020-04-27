{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module PetriAlloy where 

import Data.String.Interpolate
import Data.FileEmbed
import Data.String
import Types

petriScope :: Input -> Int
petriScope Input{places,transitions,atLeastActv,minTknsOv,maxTknsPPs,maxFlowPEdge} =
  (places+transitions)*2
  
petriLoops :: Maybe Bool -> String
petriLoops l
 | l == Just True    = "some p : Places, t : Transitions | selfLoop[p, t]"
 | l == Just False   = "no p : Places, t : Transitions | selfLoop[p, t]"
 | otherwise         = ""

petriSink :: Maybe Bool -> String
petriSink l
 | l == Just True    = "some t : Transitions | sinkTransitions[t]"
 | l == Just False   = "no t : Transitions | sinkTransitions[t]"
 | otherwise         = ""

petriSource :: Maybe Bool -> String
petriSource l
 | l == Just True    = "some t : Transitions | sourceTransitions[t]"
 | l == Just False   = "no t : Transitions | sourceTransitions[t]"
 | otherwise         = ""

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "lib/Alloy/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "lib/Alloy/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "lib/Alloy/Helpers.als")

modulePetriConcepts :: String 
modulePetriConcepts = removeLines 5 $(embedStringFile "lib/Alloy/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 4 $(embedStringFile "lib/Alloy/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

--Bigger Net needs bigger "run for x"
-- make flowSum dynamic with input

petriNetRnd :: Input -> String
petriNetRnd input@Input{places,transitions,atLeastActv,minTknsOv,maxTknsOv,maxTknsPPs,
                        minFlowOv,maxFlowOv,maxFlowPEdge,
                        selfLoops,presenceSinkTrans,presenceSourceTrans} = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred maxWeight[n : Int]{
  all weight : Nodes.flow[Nodes] | weight =< n
}

pred showNets [ps : Places, ts : Transitions, t,n : Int] {
  #Places <= #{places}
  #Transitions <= #{transitions}
  t >= #{minTknsOv}
  t <= #{maxTknsOv}
  tokensAddedOverall[t]
  perPlaceTokensAddedAtMost[#{maxTknsPPs}]
  maxWeight[#{maxFlowPEdge}]
  flowSum[Nodes,Nodes] >= #{minFlowOv}
  #{maxFlowOv} >= flowSum[Nodes,Nodes]
  n >= #{atLeastActv}
  numberActivatedTransition[n,ts]
  #{petriLoops selfLoops}
  #{petriSink presenceSinkTrans}
  #{petriSource presenceSourceTrans}
}
run showNets for #{petriScope input}

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