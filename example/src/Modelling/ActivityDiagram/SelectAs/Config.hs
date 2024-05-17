-- |

module Modelling.ActivityDiagram.SelectAs.Config where
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..))

{-|
points: 0.15
generation time: 22:00min
CPU usage: 100%
-}
task33 :: SelectASConfig
task33 = SelectASConfig {
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
  printSolution = True
  }

{-|
points: 0.15
generation time: 22:00min
CPU usage: 100%
-}
task34 :: SelectASConfig
task34 = SelectASConfig {
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
  printSolution = True
  }
