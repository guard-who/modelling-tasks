-- |

module Modelling.ActivityDiagram.FindSupportSt.Config where

import Modelling.ActivityDiagram.FindSupportST (FindSupportSTConfig(..))
import Modelling.ActivityDiagram.Config (AdConfig(..))

task41 :: FindSupportSTConfig
task41 = FindSupportSTConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (4, 4),
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
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
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
