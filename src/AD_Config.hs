{-# LANGUAGE NamedFieldPuns #-}

module AD_Config (
  ADConfig(..),
  checkADConfig
) where 

data ADConfig = ADConfig {
  actions :: Int,
  objectNodes :: Int,
  decisionNodes :: Int,
  mergeNodes :: Int,
  forkNodes :: Int,
  joinNodes :: Int,
  initialNodes :: Int,
  activityFinalNodes :: Int,
  flowFinalNodes :: Int
} deriving (Show)

checkADConfig :: ADConfig -> Maybe String
checkADConfig ADConfig {
    actions,
    objectNodes,
    decisionNodes,
    mergeNodes,
    forkNodes,
    joinNodes,
    initialNodes,
    activityFinalNodes,
    flowFinalNodes 
  }
  | actions < 0
    = Just "Number of Actions must be non-negative"
  | objectNodes < 0
    = Just "Number of Object Nodes must be non-negative"
  | actions + objectNodes <= 0
    = Just "Number of Actions and Object Nodes together must be positive"
  | decisionNodes < 0
    = Just "Number of Decision Nodes must be non-negative"
  | mergeNodes < 0
    = Just "Number of Merge Nodes must be non-negative"
  | decisionNodes /= mergeNodes
    = Just "Number of Decision Nodes and Merge Nodes must be equal"
  | forkNodes < 0
    = Just "Number of Fork Nodes must be non-negative"
  | joinNodes < 0
    = Just "Number of Join Nodes must be non-negative"
  | forkNodes /= joinNodes
    = Just "Number of Fork Nodes and Join Nodes must be equal"
  | initialNodes < 0
    = Just "Number of Initial Nodes must be non-negative"
  | initialNodes /= 1
    = Just "Number of Initial Nodes must be exactly one"
  | activityFinalNodes < 0
    = Just "Number of Activity Final Nodes must be non-negative"
  | flowFinalNodes < 0
    = Just "Number of Flow Final Nodes must be non-negative"
  | activityFinalNodes + flowFinalNodes <= 0
    = Just "Number of Activity Final Nodes and Flow Final Nodes together must be positive"
  | otherwise 
    = Nothing