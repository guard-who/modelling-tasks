{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Alloy (
  concurrencyTransition1, concurrencyTransition2,
  petriNetFindConcur, petriNetPickConcur, petriScopeBitwidth, petriScopeMaxSeq,
  petriNetRnd, renderFalse, petriNetFindConfl, petriNetPickConfl,
  ) where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
  )

import Modelling.PetriNet.Types

import Data.String.Interpolate
import Data.FileEmbed

petriScopeBitwidth :: BasicConfig -> Int
petriScopeBitwidth BasicConfig
 {places,transitions,maxFlowOverall,maxTokensOverall} = 
  floor 
     (2 + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) 
       (maximum [maxFlowOverall,maxTokensOverall,places,transitions])
     )
  
petriScopeMaxSeq :: BasicConfig -> Int
petriScopeMaxSeq BasicConfig{places,transitions} = places+transitions
  
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

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd input@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [activatedTrans : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints input}
  #{compAdvConstraints advConfig}
  
}
run showNets for exactly #{petriScopeMaxSeq input} Nodes, #{petriScopeBitwidth input} Int

|]

renderFalse :: PetriLike String -> MathConfig -> String
renderFalse
  PetriLike  {allNodes}
  MathConfig {basicTask, advTask, changeTask} = [i|module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{places}
#{transitions}

fact{
#{initialMark}

#{defaultFlow}
}

pred showFalseNets[activatedTrans : set Transitions]{
  #{compBasicConstraints basicTask}
  #{compAdvConstraints advTask}
  #{compChange changeTask}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int
|]
  where
    (ps, ts)    = M.partition isPlaceNode allNodes
    places      = unlines [extendLine p "givenPlaces" | p <- M.keys ps]
    transitions = unlines [extendLine t "givenTransitions" | t <- M.keys ts]
    initialMark = M.foldrWithKey (\k -> (++) . tokenLine k) "" $ initial <$> ps
    defaultFlow = M.foldrWithKey (\k _ -> (printFlow k ++)) "" allNodes
    printFlow :: String -> String
    printFlow x = M.foldrWithKey
      (\y flow -> (++) $ flowLine x y $ M.lookup x $ flowIn flow)
      ""
      allNodes

extendLine :: String -> String -> String
extendLine n k = [i|one sig #{n} extends #{k}{}
|]

tokenLine :: String -> Int -> String
tokenLine k l = [i|  #{k}.defaultTokens = #{l}
|]

flowLine :: String -> String -> Maybe Int -> String
flowLine from to Nothing  = [i|  no #{from}.defaultFlow[#{to}]
|]
flowLine from to (Just f) = [i|  #{from}.defaultFlow[#{to}] = #{f}
|]

conflictPredicateName :: String
conflictPredicateName = "showRelNets"

--Conflict--

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig{basicTask,advTask,changeTask} = [i|module PetriNetConfl

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{conflictPredicateName} [conflictPlace : Places, #{specCompRelation basicTask changeTask}

  #{compConflict}
  #{compAdvConstraints advTask}
  
}
run #{conflictPredicateName} for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int


|]

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl p@PickConflictConfig{basicTask = BasicConfig{atLeastActive},changeTask} = [i|module PetriNetConfl

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{conflictPredicateName} [conflictPlace : Places, defaultActivTrans : set givenTransitions, #{specCompRelation (basicTask(p :: PickConflictConfig)) changeTask}

  #{compConflict}
  #{compDefaultConstraints atLeastActive}
}
run #{conflictPredicateName} for exactly #{petriScopeMaxSeq (basicTask(p :: PickConflictConfig))} Nodes, #{petriScopeBitwidth (basicTask(p :: PickConflictConfig))} Int

|]

--Concurrency--
concurrencyPredicateName :: String
concurrencyPredicateName = "showRelNets"

petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{basicTask,advTask,changeTask} = [i|module PetriNetConcur

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{concurrencyPredicateName} [ #{specCompRelation basicTask changeTask}

  #{compConcurrency}
  #{compAdvConstraints advTask}
  
}
run #{concurrencyPredicateName} for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int


|]

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur p@PickConcurrencyConfig{basicTask = BasicConfig{atLeastActive},changeTask} = [i|module PetriNetConcur

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{concurrencyPredicateName} [defaultActivTrans : set givenTransitions, #{specCompRelation (basicTask(p :: PickConcurrencyConfig)) changeTask}

  #{compConcurrency}
  #{compDefaultConstraints atLeastActive}
}
run #{concurrencyPredicateName} for exactly #{petriScopeMaxSeq (basicTask(p :: PickConcurrencyConfig))} Nodes, #{petriScopeBitwidth (basicTask(p :: PickConcurrencyConfig))} Int

|]

----------------------"Building-Kit"----------------------------

-- Needs: activatedTrans : set Transitions
compBasicConstraints :: BasicConfig -> String
compBasicConstraints BasicConfig
                        {atLeastActive,minTokensOverall
                        ,maxTokensOverall,maxTokensPerPlace
                        , minFlowOverall,maxFlowOverall,maxFlowPerEdge
                        } = [i|
  let t = tokenSum[Places] | t >= #{minTokensOverall} and t <= #{maxTokensOverall}
  all p : Places | p.tokens =< #{maxTokensPerPlace}
  all weight : Nodes.flow[Nodes] | weight =< #{maxFlowPerEdge}
  let flow = flowSum[Nodes,Nodes] | flow >= #{minFlowOverall} and #{maxFlowOverall} >= flow
  #activatedTrans >= #{atLeastActive}
  theActivatedTransitions[activatedTrans]
  graphIsConnected[]
  
|]

compAdvConstraints :: AdvConfig -> String
compAdvConstraints AdvConfig
                        { presenceOfSelfLoops, presenceOfSinkTransitions
                        , presenceOfSourceTransitions
                        } = [i| 
  #{maybe "" petriLoops presenceOfSelfLoops}
  #{maybe "" petriSink presenceOfSinkTransitions}
  #{maybe "" petriSource presenceOfSourceTransitions}
|]

--Needs: defaultActivTrans : set Transitions
compDefaultConstraints :: Int -> String
compDefaultConstraints atLeastActive = [i|
  defaultGraphIsConnected[]
  #defaultActivTrans >= #{atLeastActive}
  theActivatedDefaultTransitions[defaultActivTrans]
|]

compChange :: ChangeConfig -> String
compChange ChangeConfig
                  {flowChangeOverall, maxFlowChangePerEdge
                  , tokenChangeOverall, maxTokenChangePerPlace
                  } = [i|
  flowChangeAbsolutesSum[Nodes,Nodes] = #{flowChangeOverall}
  maxFlowChangePerEdge [#{maxFlowChangePerEdge}]
  tokenChangeAbsolutesSum[Places] = #{tokenChangeOverall}
  maxTokenChangePerPlace [#{maxTokenChangePerPlace}]
|]

concurrencyTransition1 :: String
concurrencyTransition1 = "$" ++ concurrencyPredicateName ++ "_" ++ transition1

concurrencyTransition2 :: String
concurrencyTransition2 = "$" ++ concurrencyPredicateName ++ "_" ++ transition2

transition1 :: String
transition1 = "concurTrans1"

transition2 :: String
transition2 = "concurTrans2"

compConcurrency :: String
compConcurrency = [i|
  no x,y : givenTransitions | concurrentDefault[x+y] and x != y
  some #{t1}, #{t2} : Transitions | relatedTransitions = #{t1} + #{t2}
  and #{t1} != #{t2}
  and concurrent [#{t1}+#{t2}]
  and all u,v : Transitions | concurrent[u+v] and u != v implies #{t1} + #{t2} = u + v
|]
  where
    t1 = transition1
    t2 = transition2

--Needs: relatedTransitions: set Transitions, conflictTrans1,conflictTrans2 : Transitions, conflictPlace : Places
compConflict :: String
compConflict = [i|
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  some conflictTrans1, conflictTrans2 : Transitions | relatedTransitions = conflictTrans1 + conflictTrans2 
  and conflict [conflictTrans1, conflictTrans2, conflictPlace] and all u,v : Transitions, q : Places 
    | conflict[u,v,q] implies conflictTrans1 + conflictTrans2 = u + v
  
|]

specCompRelation :: BasicConfig -> ChangeConfig -> String
specCompRelation basic@BasicConfig{places,transitions} change = [i|
activatedTrans,relatedTransitions: set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints basic}
  #{compChange change}
|]
