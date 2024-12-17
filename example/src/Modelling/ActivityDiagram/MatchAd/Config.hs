-- |

module Modelling.ActivityDiagram.MatchAd.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchAd (MatchAdConfig(..))

{-|
points: 0.15
-}
task2023_31 :: MatchAdConfig
task2023_31 = MatchAdConfig {
  adConfig = AdConfig {
    actionLimits = (5, 6),
    objectNodeLimits = (5, 6),
    maxNamedNodes = 11,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
    },
  maxInstances = Just 500,
  hideBranchConditions = False,
  noActivityFinalInForkBlocks = Just False,
  printSolution = True
  }

{-|
points: 0.15
-}
task2023_32 :: MatchAdConfig
task2023_32 = MatchAdConfig {
  adConfig = AdConfig {
    actionLimits = (5, 6),
    objectNodeLimits = (5, 6),
    maxNamedNodes = 11,
    decisionMergePairs = 1,
    forkJoinPairs = 2,
    activityFinalNodes = 1,
    flowFinalNodes = 2,
    cycles = 1
    },
  maxInstances = Just 500,
  hideBranchConditions = True,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }

{-|
points: 0.15
-}
task2024_37 :: MatchAdConfig
task2024_37 = task2023_31

{-|
points: 0.15
-}
task2024_38 :: MatchAdConfig
task2024_38 = task2023_32
