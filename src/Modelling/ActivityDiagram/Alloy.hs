{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Modelling.ActivityDiagram.Alloy(
  adConfigBitWidth,
  adConfigScope,
  adConfigToAlloy,
  adConfigToAlloy', -- only for test cases
  modulePetriNet,
  moduleActionSequencesRules
) where

import Modelling.ActivityDiagram.Config (AdConfig (..))

import Data.FileEmbed                   (embedStringFile)
import Data.String.Interpolate          (i)

moduleComponentsSig :: String
moduleComponentsSig = removeLines 1 $(embedStringFile "alloy/ad/components.als")

moduleInitialNodeRules :: String
moduleInitialNodeRules =
  removeLines 3 $(embedStringFile "alloy/ad/initialNodeRules.als")

moduleNameRules :: String
moduleNameRules = removeLines 3 $(embedStringFile "alloy/ad/nameRules.als")

moduleReachabilityRules :: String
moduleReachabilityRules =
  removeLines 3 $(embedStringFile "alloy/ad/reachabilityRules.als")

modulePlantUMLSig :: String
modulePlantUMLSig = removeLines 3 $(embedStringFile "alloy/ad/plantUml.als")

moduleExerciseRules :: String
moduleExerciseRules =
  removeLines 3 $(embedStringFile "alloy/ad/exerciseRules.als")

modulePetriNet :: String
modulePetriNet = removeLines 3 $(embedStringFile "alloy/ad/petriNet.als")

moduleActionSequencesRules :: String
moduleActionSequencesRules =
  removeLines 3 $(embedStringFile "alloy/ad/actionSequencesRules.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

adConfigToAlloy :: String -> String -> AdConfig -> String
adConfigToAlloy modules predicates adConf = adConfigToAlloy'
  (adConfigScope adConf)
  (adConfigBitWidth adConf)
  modules
  predicates
  adConf

adConfigToAlloy' :: Int -> Int -> String -> String -> AdConfig -> String
adConfigToAlloy' scope bitWidth modules predicates AdConfig {
    actionLimits,
    objectNodeLimits,
    maxNamedNodes,
    decisionMergePairs,
    forkJoinPairs,
    activityFinalNodes,
    flowFinalNodes,
    cycles
  } =
  [i|module MatchPetri
    #{moduleComponentsSig}
    #{moduleInitialNodeRules}
    #{moduleNameRules}
    #{moduleReachabilityRules}
    #{modulePlantUMLSig}
    #{moduleExerciseRules}
    #{modules}

    #{singletonActions}
    #{singletonObjectNodes}

    pred showAd {
      #{predicates}
    }

    run showAd for #{scope} but #{bitWidth} Int, #{snd actionLimits} ActionNodes,
      #{snd objectNodeLimits} ObjectNodes, #{maxNamedNodes} ActionObjectNodes,
      #{snd actionLimits + snd objectNodeLimits} ComponentNames,
      exactly #{decisionMergePairs} DecisionNodes, exactly #{decisionMergePairs} MergeNodes,
      #{2 * decisionMergePairs} GuardNames, exactly #{forkJoinPairs} ForkNodes, exactly #{forkJoinPairs} JoinNodes,
      exactly 1 InitialNodes, exactly #{activityFinalNodes} ActivityFinalNodes, exactly #{flowFinalNodes} FlowFinalNodes,
      exactly #{cycles} PlantUMLRepeatBlocks, exactly #{decisionMergePairs - cycles} PlantUmlIfElseBlocks,
      exactly #{forkJoinPairs} PlantUMLForkBlocks
  |]
  where
    singletonActions = unlines $ map
      (\x -> [i| one sig A#{x} extends ActionNodes {}|])
      [1 .. fst actionLimits]
    singletonObjectNodes = unlines $ map
      (\x -> [i| one sig O#{x} extends ObjectNodes {}|])
      [1 .. fst objectNodeLimits]

adConfigScope :: AdConfig -> Int
adConfigScope AdConfig {
    maxNamedNodes,
    decisionMergePairs,
    forkJoinPairs
  } = 1 + maxNamedNodes + 3 * decisionMergePairs + 4 * forkJoinPairs

{-
 As of now, the highest Int-Value used in the Alloy Specification is 3 (#bodies in ForkBlocks),
 therefore 3 Bit (Two's Complement) should be enough.
 If this number is made configurable or the specification is changed to use larger Int values,
 this should be adapted.
-}
adConfigBitWidth :: AdConfig -> Int
adConfigBitWidth = const 3
