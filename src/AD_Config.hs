{-# LANGUAGE NamedFieldPuns #-}

module AD_Config (
  ADConfig(..),
  defaultADConfig,
  checkADConfig
) where 

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
    = Just "Number of Activity Final Nodes and Flow Final Nodes together must be positive"
  | cycles > decisionMergePairs
    = Just "Number of Cycles must be less or equal to the number of Decision and Merge pairs"
  | otherwise 
    = Nothing