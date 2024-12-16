-- |

module Modelling.ActivityDiagram.EnterAs.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.EnterAS (EnterASConfig(..))

{-|
points: 0.15
generation time per instance: 3:30min
CPU usage: 100%
-}
task35 :: EnterASConfig
task35 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (12, 12),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 17,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just True,
  answerLength = (10, 10),
  printSolution = True
  }

{-|
points: 0.15
generation time per instance: 5:00min
CPU usage: 100%
-}
task36 :: EnterASConfig
task36 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (14, 14),
    objectNodeLimits = (6, 6),
    maxNamedNodes = 20,
    decisionMergePairs = 1,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just False,
  answerLength = (14, 14),
  printSolution = True
  }
