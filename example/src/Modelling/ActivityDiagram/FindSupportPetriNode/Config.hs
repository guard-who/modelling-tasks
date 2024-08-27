-- |

module Modelling.ActivityDiagram.FindSupportPetriNode.Config where

import Modelling.ActivityDiagram.FindSupportPetriNode (
  FindSupportPetriNodeConfig (..),
  )
import Modelling.ActivityDiagram.Config (AdConfig(..))

{-|
points: 0.15
generation time: 2:00min
CPU usage: 120%
-}
task41 :: FindSupportPetriNodeConfig
task41 = FindSupportPetriNodeConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 10,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 0
    },
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  activityFinalsExist = Just False,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

{-|
points: 0.15
generation time: 3:30min
CPU usage: 100%
-}
task42 :: FindSupportPetriNodeConfig
task42 = FindSupportPetriNodeConfig {
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
