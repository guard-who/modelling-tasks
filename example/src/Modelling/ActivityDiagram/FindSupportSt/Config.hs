-- |

module Modelling.ActivityDiagram.FindSupportSt.Config where

import Modelling.ActivityDiagram.FindSupportST (FindSupportSTConfig(..))
import Modelling.ActivityDiagram.Config (AdConfig(..))

task41 :: FindSupportSTConfig
task41 = FindSupportSTConfig {
  adConfig = AdConfig {
    minActions = 6,
    maxActions = 6,
    minObjectNodes = 4,
    maxObjectNodes = 4,
    maxNamedNodes = 10,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 0
    },
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

task42 :: FindSupportSTConfig
task42 = FindSupportSTConfig {
  adConfig = AdConfig {
    minActions = 8,
    maxActions = 8,
    minObjectNodes = 4,
    maxObjectNodes = 4,
    maxNamedNodes = 12,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 0,
    cycles = 2
    },
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }
