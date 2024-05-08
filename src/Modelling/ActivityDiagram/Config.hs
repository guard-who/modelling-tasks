{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.Config (
  AdConfig (..),
  defaultAdConfig,
  checkAdConfig,
  adConfigBitWidth,
  adConfigToAlloy,
  adConfigToAlloy',
  adConfigScope,
) where

import Modelling.ActivityDiagram.Alloy (moduleComponentsSig, moduleInitialNodeRules, moduleNameRules, moduleReachabilityRules, modulePlantUMLSig, moduleExerciseRules)

import Data.String.Interpolate ( i )
import GHC.Generics (Generic)

data AdConfig = AdConfig {
  actionLimits :: !(Int, Int),
  objectNodeLimits :: !(Int, Int),
  maxNamedNodes :: Int,
  decisionMergePairs :: Int,
  forkJoinPairs :: Int,
  activityFinalNodes :: Int,
  flowFinalNodes :: Int,
  cycles :: Int
} deriving (Generic, Read, Show)

defaultAdConfig :: AdConfig
defaultAdConfig = AdConfig
  { actionLimits = (4, 4),
    objectNodeLimits = (1, 1),
    maxNamedNodes = 5,
    decisionMergePairs = 1,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
  }

checkAdConfig :: AdConfig -> Maybe String
checkAdConfig AdConfig {
    actionLimits,
    objectNodeLimits,
    maxNamedNodes,
    decisionMergePairs,
    forkJoinPairs,
    activityFinalNodes,
    flowFinalNodes,
    cycles
  }
  | fst actionLimits < 0
    = Just "Minimum number of Actions must be non-negative"
  | uncurry (>) actionLimits
    = Just "Maximal number of Actions must not be larger than the minimum number"
  | fst objectNodeLimits < 0
    = Just "Minimum number of Object Nodes must be non-negative"
  | uncurry (>) objectNodeLimits
    = Just "Maximal number of Object Nodes must not be larger than the minimum number"
  | fst actionLimits + fst objectNodeLimits <= 0
    = Just "Minimum number of Actions and Object Nodes together must be positive"
  | fst actionLimits + fst objectNodeLimits > maxNamedNodes
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
 If this number is made configurable or the specification is changed to use lartger Int values,
 this should be adapted.
-}
adConfigBitWidth :: AdConfig -> Int
adConfigBitWidth = const 3
