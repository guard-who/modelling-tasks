-- |

module Modelling.ActivityDiagram.SelectAs.Config where
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..))

{-|
points: 0.15
average generation time per instance: 22:00min
CPU usage: 100%
-}
task2023_33 :: SelectASConfig
task2023_33 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (10, 10),
    objectNodeLimits = (2, 2),
    maxNamedNodes = 12,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 2
    },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Just True,
  numberOfWrongAnswers = 6,
  answerLength = (10, 10),
  printSolution = True,
  extraText = Nothing
  }

{-|
points: 0.15
average generation time per instance: 22:00min
CPU usage: 100%
-}
task2023_34 :: SelectASConfig
task2023_34 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (10, 10),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 15,
    decisionMergePairs = 2,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 9,
  answerLength = (9, 9),
  printSolution = True,
  extraText = Nothing
  }

{-|
points: 0.15
average generation time per instance: 25:33min
CPU usage: 101%
-}
task2024_39 :: SelectASConfig
task2024_39 = task2023_33

{-|
points: 0.15
average generation time per instance: 24:09min
CPU usage: 101%
-}
task2024_40 :: SelectASConfig
task2024_40 = task2023_34
