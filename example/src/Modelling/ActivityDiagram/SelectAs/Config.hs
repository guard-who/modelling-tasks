-- |

module Modelling.ActivityDiagram.SelectAs.Config where
import Modelling.ActivityDiagram.Config (ADConfig(..))
import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..))

task33 :: SelectASConfig
task33 = SelectASConfig {
  adConfig = ADConfig {
    minActions = 10,
    maxActions = 10,
    minObjectNodes = 2,
    maxObjectNodes = 2,
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
  minAnswerLength = 10,
  maxAnswerLength = 10,
  printSolution = True
  }

task34 :: SelectASConfig
task34 = SelectASConfig {
  adConfig = ADConfig {
    minActions = 10,
    maxActions = 10,
    minObjectNodes = 5,
    maxObjectNodes = 5,
    maxNamedNodes = 15,
    decisionMergePairs = 2,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 3,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 9,
  minAnswerLength = 9,
  maxAnswerLength = 9,
  printSolution = True
  }
