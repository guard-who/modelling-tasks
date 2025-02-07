-- |

module Modelling.ActivityDiagram.EnterAs.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.EnterAS (EnterASConfig(..))

{-|
points: 0.15
average generation time per instance: 3:30min
CPU usage: 100%
-}
task2023_35 :: EnterASConfig
task2023_35 = EnterASConfig {
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
average generation time per instance: 5:00min
CPU usage: 100%
-}
task2023_36 :: EnterASConfig
task2023_36 = EnterASConfig {
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

{-|
points: 0.15
average generation time per instance: 3:57min
CPU usage: 109%
-}
task2024_41 :: EnterASConfig
task2024_41 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (12, 12),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 17,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 2
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just True,
  answerLength = (11, 11),
  printSolution = True
  }

{-|
points: 0.15
average generation time per instance: 3:23min
CPU usage: 113%
-}
task2024_42 :: EnterASConfig
task2024_42 = task2023_36

{-|
points: 0.08
average generation time per instance: ?:??min
CPU usage: ???%
-}
task2024_68 :: EnterASConfig
task2024_68 = task2024_41

{-|
points: 0.08
average generation time per instance: ?:??min
CPU usage: ???%
-}
task2024_69 :: EnterASConfig
task2024_69 = task2023_36
