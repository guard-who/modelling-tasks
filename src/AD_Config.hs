{-# LANGUAGE NamedFieldPuns #-}

module AD_Config (
  ADConfig(..),
  checkADConfig
) where 

data ADConfig = ADConfig {
  actions :: Int,
  objectNodes :: Int,
  decisionMergePairs :: Int,
  forkJoinPairs :: Int,
  activityFinalNodes :: Int,
  flowFinalNodes :: Int,
  cycles :: Int
} deriving (Show)

checkADConfig :: ADConfig -> Maybe String
checkADConfig ADConfig {
    actions,
    objectNodes,
    decisionMergePairs,
    forkJoinPairs,
    activityFinalNodes,
    flowFinalNodes,
    cycles
  }
  | actions < 0
    = Just "Number of Actions must be non-negative"
  | objectNodes < 0
    = Just "Number of Object Nodes must be non-negative"
  | actions + objectNodes <= 0
    = Just "Number of Actions and Object Nodes together must be positive"
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