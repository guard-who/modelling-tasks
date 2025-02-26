-- |

module Modelling.ActivityDiagram.FindAuxiliaryPetriNodes.Config where

import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  FindAuxiliaryPetriNodesConfig (..),
  )
import Modelling.ActivityDiagram.Config (AdConfig(..))

{-|
points: 0.15
average generation time per instance: 2:00min
CPU usage: 120%
-}
task2023_41 :: FindAuxiliaryPetriNodesConfig
task2023_41 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

{-|
points: 0.15
average generation time per instance: 3:30min
CPU usage: 100%
-}
task2023_42 :: FindAuxiliaryPetriNodesConfig
task2023_42 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

{-|
points: 0.15
average generation time per instance: 2:24min
CPU usage: 104%
-}
task2024_47 :: FindAuxiliaryPetriNodesConfig
task2024_47 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (21, Just 27),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

{-|
points: 0.15
average generation time per instance: 3:51min
CPU usage: 108%
-}
task2024_48 :: FindAuxiliaryPetriNodesConfig
task2024_48 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (31, Just 41),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Nothing,
  printSolution = True
  }

{-|
points: 0.08
average generation time per instance: 1:51min
CPU usage: 122%
-}
task2024_72 :: FindAuxiliaryPetriNodesConfig
task2024_72 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (21, Just 27),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Just True,
  printSolution = True
  }

{-|
points: 0.08
average generation time per instance: 3:28min
CPU usage: 112%
-}
task2024_73 :: FindAuxiliaryPetriNodesConfig
task2024_73 = FindAuxiliaryPetriNodesConfig {
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
  countOfPetriNodesBounds = (31, Just 41),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  avoidAddingSinksForFinals = Just False,
  printSolution = True
  }
