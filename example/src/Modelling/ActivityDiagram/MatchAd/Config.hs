-- |

module Modelling.ActivityDiagram.MatchAd.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchAd (MatchAdConfig(..))

task31 :: MatchAdConfig
task31 = MatchAdConfig {
  adConfig = AdConfig {
    minActions = 5,
    maxActions = 6,
    minObjectNodes = 5,
    maxObjectNodes = 6,
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

task32 :: MatchAdConfig
task32 = MatchAdConfig {
  adConfig = AdConfig {
    minActions = 5,
    maxActions = 6,
    minObjectNodes = 5,
    maxObjectNodes = 6,
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
