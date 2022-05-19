module AD_Config (
  ADConfig(..)
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