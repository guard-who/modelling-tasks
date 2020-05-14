{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.Alloy 
  (petriNetRnd,renderFalse,petriNetConfl) where

import Modelling.PetriNet.Types

import Data.String.Interpolate
import Data.FileEmbed

petriScope :: PetriBasicConfig -> Int
petriScope PetriBasicConfig{places,transitions,maxFlowPerEdge} = max
  (ceiling ( 2
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) places
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) transitions
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) maxFlowPerEdge
  ))
  (places+transitions)
  
petriLoops :: Bool -> String
petriLoops = \case
 True  -> "some n : Nodes | selfLoop[n]"
 False -> "no n : Nodes | selfLoop[n]"

petriSink :: Bool -> String
petriSink = \case
 True  -> "some t : Transitions | sinkTransitions[t]"
 False -> "no t : Transitions | sinkTransitions[t]"

petriSource :: Bool -> String
petriSource = \case
 True  -> "some t : Transitions | sourceTransitions[t]"
 False -> "no t : Transitions | sourceTransitions[t]"

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

petriNetConstraints :: PetriBasicConfig -> String
petriNetConstraints PetriBasicConfig{atLeastActive,minTokensOverall,maxTokensOverall,maxTokensPerPlace,
                        minFlowOverall,maxFlowOverall,maxFlowPerEdge,
                        presenceOfSelfLoops,presenceOfSinkTransitions,presenceOfSourceTransitions} = [i|
  let t = tokenSum[Places] | t >= #{minTokensOverall} and t <= #{maxTokensOverall}
  all p : Places | p.tokens =< #{maxTokensPerPlace}
  all weight : Nodes.flow[Nodes] | weight =< #{maxFlowPerEdge}
  let flow = flowSum[Nodes,Nodes] | flow >= #{minFlowOverall} and #{maxFlowOverall} >= flow
  #ts >= #{atLeastActive}
  theActivatedTransitions[ts]
  graphIsConnected[]
  #{maybe "" petriLoops presenceOfSelfLoops}
  #{maybe "" petriSink presenceOfSinkTransitions}
  #{maybe "" petriSource presenceOfSourceTransitions}
|]

petriNetRnd :: PetriBasicConfig -> String
petriNetRnd input@PetriBasicConfig{places,transitions} = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [ts : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{petriNetConstraints input}
  
}
run showNets for #{petriScope input}

|]

renderFalse :: Petri -> PetriTask1Config -> String
renderFalse Petri{initialMarking,trans}
    PetriTask1Config{basicTask1,flowChangeOverall, maxFlowChangePerEdge, tokenChangeOverall, maxTokenChangePerPlace} = [i| module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{givPlaces (length initialMarking)}
#{givTrans (length trans)}

fact{
  #{initialMark 1 initialMarking}

  #{defFlow 1 trans}
}

pred showFalseNets[ts : set Transitions]{
  #{petriNetConstraints basicTask1}
  flowChangeAbsolutesSum[Nodes,Nodes] = #{flowChangeOverall}
  maxFlowChangePerEdge [#{maxFlowChangePerEdge}]
  tokenChangeAbsolutesSum[Places] = #{tokenChangeOverall}
  maxTokenChangePerPlace [#{maxTokenChangePerPlace}]

}

run showFalseNets for #{petriScope basicTask1}

|]

petriNetConfl :: PetriBasicConfig -> String
petriNetConfl input@PetriBasicConfig{places,transitions} = [i|module PetriNetConfl

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showConflNets [ts,tc1,tc2 : set Transitions, pc : Places] {
  #Places = #{places}
  #Transitions = #{transitions}
  conflict[tc1, tc2, pc] and all u,v : Transitions, q : Places | conflict[u,v,q] implies tc1 + tc2 = u + v
  #{petriNetConstraints input}
  
}
run showConflNets for #{petriScope input}

|]

givPlaces :: Int -> String
givPlaces 0 = ""
givPlaces p = "one sig S"++show p++" extends givenPlaces{} \n"++ givPlaces (p-1)

givTrans :: Int -> String
givTrans 0 = ""
givTrans t = "one sig T"++show t++" extends givenTransitions{} \n"++ givTrans (t-1)

initialMark ::Int -> Marking -> String
initialMark _ []      = ""
initialMark iM (m:rm) ="S"++ show iM ++".defaultTokens = "++ show m ++"\n  " ++ initialMark (iM+1) rm

defFlow :: Int -> [Transition] -> String
defFlow _ []            = ""
defFlow iT ((pr,po):rt) = flowPre iT 1 pr ++ flowPost iT 1 po ++ defFlow (iT+1) rt

flowPre :: Int -> Int -> [Int] -> String
flowPre _ _ [] = ""
flowPre iT iM (m:rm)
 | m == 0     = "no S"++ show iM ++".defaultFlow[T"++ show iT ++"]\n  " ++ flowPre iT (iM+1) rm
 | otherwise  = "S"++ show iM ++".defaultFlow[T"++ show iT ++"] = "++ show m ++"\n  "
                       ++ flowPre iT (iM+1) rm

flowPost :: Int -> Int -> [Int] -> String
flowPost _ _ [] = ""
flowPost iT iM (m:rm)
 | m == 0     = "no T"++ show iT ++".defaultFlow[S"++ show iM ++"]\n  " ++ flowPost iT (iM+1) rm
 | otherwise  = "T"++ show iT ++".defaultFlow[S"++ show iM ++"] = "++ show m ++"\n  "
                        ++ flowPost iT (iM+1) rm
