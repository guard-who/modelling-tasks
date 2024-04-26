-- |

module Modelling.ActivityDiagram.EnterAs.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.EnterAS (EnterASConfig(..))

task35 :: EnterASConfig
task35 = EnterASConfig {
  adConfig = AdConfig {
    minActions = 12,
    maxActions = 12,
    minObjectNodes = 5,
    maxObjectNodes = 5,
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
  minAnswerLength = 10,
  maxAnswerLength = 10,
  printSolution = True
  }

task36 :: EnterASConfig
task36 = EnterASConfig {
  adConfig = AdConfig {
    minActions = 14,
    maxActions = 14,
    minObjectNodes = 6,
    maxObjectNodes = 6,
    maxNamedNodes = 20,
    decisionMergePairs = 2,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just False,
  minAnswerLength = 11,
  maxAnswerLength = 11,
  printSolution = True
  }
