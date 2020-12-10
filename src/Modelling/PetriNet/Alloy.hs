{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Alloy (
  concurrencyTransition1, concurrencyTransition2,
  conflictPlaces1, conflictTransition1, conflictTransition2,
  getAlloyInstances,
  petriNetFindConcur, petriNetFindConfl, petriNetPickConcur, petriNetPickConfl,
  petriNetRnd, petriScopeBitwidth, petriScopeMaxSeq, renderFalse,
  ) where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
  )

import Modelling.PetriNet.Types

import Control.Monad                    (when)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.FileEmbed
import Data.String.Interpolate
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig, getInstancesWith,
  )

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
conflictPredicateName = "showConflict"

--Conflict--

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig {
  basicConfig,
  advConfig,
  changeConfig,
  uniqueConflictPlace
  } = [i|module PetriNetConfl

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{conflictPredicateName} [#{p} : some Places, #{specCompRelation t1 t2 basicConfig changeConfig}

  #{compConflict uniqueConflictPlace t1 t2 p}
  #{compAdvConstraints advConfig}
  
}
run #{conflictPredicateName} for exactly #{petriScopeMaxSeq basicConfig} Nodes, #{petriScopeBitwidth basicConfig} Int


|]
  where
    t1 = transition1
    t2 = transition2
    p  = places1

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl p@PickConflictConfig {
  basicConfig = BasicConfig {atLeastActive},
  changeConfig,
  uniqueConflictPlace
  } = [i|module PetriNetConfl

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{conflictPredicateName} [#{pl} : some Places, defaultActivTrans : set givenTransitions, #{specCompRelation t1 t2 (basicConfig(p :: PickConflictConfig)) changeConfig}

  #{compConflict uniqueConflictPlace t1 t2 pl}
  #{compDefaultConstraints atLeastActive}
}
run #{conflictPredicateName} for exactly #{petriScopeMaxSeq (basicConfig(p :: PickConflictConfig))} Nodes, #{petriScopeBitwidth (basicConfig(p :: PickConflictConfig))} Int

|]
  where
    t1 = transition1
    t2 = transition2
    pl = places1

--Concurrency--
concurrencyPredicateName :: String
concurrencyPredicateName = "showConcurrency"

petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{
  basicConfig,
  advConfig,
  changeConfig
  } = [i|module PetriNetConcur

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{concurrencyPredicateName} [ #{specCompRelation t1 t2 basicConfig changeConfig}

  #{compConcurrency t1 t2}
  #{compAdvConstraints advConfig}
  
}
run #{concurrencyPredicateName} for exactly #{petriScopeMaxSeq basicConfig} Nodes, #{petriScopeBitwidth basicConfig} Int


|]
  where
    t1 = transition1
    t2 = transition2

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur p@PickConcurrencyConfig{
  basicConfig = BasicConfig{atLeastActive}
  ,changeConfig
  } = [i|module PetriNetConcur

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{concurrencyPredicateName} [defaultActivTrans : set givenTransitions, #{specCompRelation t1 t2 (basicConfig(p :: PickConcurrencyConfig)) changeConfig}

  #{compConcurrency t1 t2}
  #{compDefaultConstraints atLeastActive}
}
run #{concurrencyPredicateName} for exactly #{petriScopeMaxSeq (basicConfig(p :: PickConcurrencyConfig))} Nodes, #{petriScopeBitwidth (basicConfig(p :: PickConcurrencyConfig))} Int

|]
  where
    t1 = transition1
    t2 = transition2

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

skolemVariable :: String -> String -> String
skolemVariable x y = '$' : x ++ '_' : y

concurrencyTransition1 :: String
concurrencyTransition1 = skolemVariable concurrencyPredicateName transition1

concurrencyTransition2 :: String
concurrencyTransition2 = skolemVariable concurrencyPredicateName transition2

conflictPlaces1 :: String
conflictPlaces1 = skolemVariable conflictPredicateName places1

conflictTransition1 :: String
conflictTransition1 = skolemVariable conflictPredicateName transition1

conflictTransition2 :: String
conflictTransition2 = skolemVariable conflictPredicateName transition2

transition1 :: String
transition1 = "transition1"

transition2 :: String
transition2 = "transition2"

places1 :: String
places1 = "places"

compConcurrency :: String -> String -> String
compConcurrency t1 t2 = [i|
  no x,y : givenTransitions | concurrentDefault[x+y] and x != y
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
    and all u,v : Transitions |
      concurrent[u + v] and u != v implies #{t1} + #{t2} = u + v
|]

compConflict :: Maybe Bool -> String -> String -> String -> String
compConflict muniquePlace t1 t2 p = [i|
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  #{multiplePlaces}
  all q : #{p} | conflict[#{t1}, #{t2}, q]
  all u,v : Transitions | no q : (Places - #{p}) | conflict[u,v,q]
  all u,v : Transitions, q : Places |
    conflict[u,v,q] implies #{t1} + #{t2} = u + v #{uniquePlace}
|]
  where
    uniquePlace
      | muniquePlace == Just True
      = [i|and q = #{p}|]
      | otherwise
      = ""
    multiplePlaces
      | muniquePlace == Just False
      = [i|\##{p} > 1|]
      | otherwise
      = ""

specCompRelation :: String -> String -> BasicConfig -> ChangeConfig -> String
specCompRelation t1 t2 basic@BasicConfig{places,transitions} change = [i|
activatedTrans : set Transitions, #{t1}, #{t2} : Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints basic}
  #{compChange change}
|]

getAlloyInstances
  :: CallAlloyConfig
  -> String
  -> ExceptT String IO [AlloyInstance]
getAlloyInstances config alloy = do
  list <- lift $ getInstancesWith config alloy
  when (null list) $ except $ Left "no instance available"
  return list
