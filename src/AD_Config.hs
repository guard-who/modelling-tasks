{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_Config (
  ADConfig(..),
  defaultADConfig,
  checkADConfig,
  adConfigToAlloy
) where

import AD_Alloy (moduleComponentsSig, moduleInitialNodeRules, moduleNameRules, moduleReachabilityRules, modulePlantUMLSig, moduleExerciseRules)

import Data.String.Interpolate ( i )


data ADConfig = ADConfig {
  minActions :: Int,
  maxActions :: Int,
  minObjectNodes :: Int,
  maxObjectNodes :: Int,
  maxNamedNodes :: Int,
  decisionMergePairs :: Int,
  forkJoinPairs :: Int,
  activityFinalNodes :: Int,
  flowFinalNodes :: Int,
  cycles :: Int
} deriving (Show)

defaultADConfig :: ADConfig
defaultADConfig = ADConfig
  { minActions = 3,
    maxActions = 5,
    minObjectNodes = 3,
    maxObjectNodes = 5,
    maxNamedNodes = 8,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
  }

checkADConfig :: ADConfig -> Maybe String
checkADConfig ADConfig {
    minActions,
    maxActions,
    minObjectNodes,
    maxObjectNodes,
    maxNamedNodes,
    decisionMergePairs,
    forkJoinPairs,
    activityFinalNodes,
    flowFinalNodes,
    cycles
  }
  | minActions < 0
    = Just "Minimum number of Actions must be non-negative"
  | maxActions < minActions
    = Just "Maximal number of Actions must not be larger than the minimum number"
  | minObjectNodes < 0
    = Just "Minimum number of Object Nodes must be non-negative"
  | maxObjectNodes < minObjectNodes
    = Just "Maximal number of Object Nodes must not be larger than the minimum number"
  | minActions + minObjectNodes <= 0
    = Just "Minimum number of Actions and Object Nodes together must be positive"
  | minActions + minObjectNodes > maxNamedNodes
    = Just "Minimal number of Actions and Object Nodes together must not be larger than maximum number of Named Nodes"
  | decisionMergePairs < 0
    = Just "Number of Decision and Merge pairs must be non-negative"
  | forkJoinPairs < 0
    = Just "Number of Fork and Join pairs must be non-negative"
  | activityFinalNodes < 0
    = Just "Number of Activity Final Nodes must be non-negative"
  | flowFinalNodes < 0
    = Just "Number of Flow Final Nodes must be non-negative"
  | activityFinalNodes + flowFinalNodes <= 0
    = Just "Total number of Final Nodes must be positive"
  | activityFinalNodes + flowFinalNodes > 1 + forkJoinPairs
    = Just "Total number of Final Nodes must be less or equal to the number of Fork and Join pairs plus one"
  | cycles > decisionMergePairs
    = Just "Number of Cycles must be less or equal to the number of Decision and Merge pairs"
  | otherwise
    = Nothing


adConfigToAlloy :: String -> String -> ADConfig -> String
adConfigToAlloy modules preds adConf@ADConfig {
    minActions,
    maxActions,
    minObjectNodes,
    maxObjectNodes,
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

    pred showAD {
      #{preds}
    }

    run showAD for #{adConfigScope adConf} but 6 Int, #{maxActions} ActionNodes,
      #{maxObjectNodes} ObjectNodes, #{maxNamedNodes} ActionObjectNodes, #{maxActions + maxObjectNodes} ComponentNames,
      exactly #{decisionMergePairs} DecisionNodes, exactly #{decisionMergePairs} MergeNodes,
      #{2 * decisionMergePairs} GuardNames, exactly #{forkJoinPairs} ForkNodes, exactly #{forkJoinPairs} JoinNodes,
      exactly 1 InitialNodes, exactly #{activityFinalNodes} ActivityFinalNodes, exactly #{flowFinalNodes} FlowFinalNodes,
      exactly #{cycles} PlantUMLRepeatBlocks, exactly #{decisionMergePairs - cycles} PlantUMLIfElseBlocks,
      exactly #{forkJoinPairs} PlantUMLForkBlocks
  |]
  where
    singletonActions =
      unlines $ map (\x -> [i| one sig A#{x} extends ActionNodes \{\}|]) [1..minActions]
    singletonObjectNodes =
      unlines $ map (\x -> [i| one sig O#{x} extends ObjectNodes \{\}|]) [1..minObjectNodes]

adConfigScope :: ADConfig -> Int
adConfigScope ADConfig {
    maxActions,
    maxObjectNodes,
    decisionMergePairs,
    forkJoinPairs
  } = 1 + maxActions + maxObjectNodes + 3 * decisionMergePairs + 4 * forkJoinPairs