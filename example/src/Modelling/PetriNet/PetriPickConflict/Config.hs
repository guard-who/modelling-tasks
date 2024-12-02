-- |

module Modelling.PetriNet.PetriPickConflict.Config where

import Modelling.PetriNet.Types (
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  ConflictConfig (..),
  GraphConfig (..),
  PickConflictConfig (..),
  )
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.1
generation time: 3:30min
CPU usage: 110%
-}
task2023_22 :: PickConflictConfig
task2023_22 = PickConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 14) ,
    maxTokensPerPlace = 1,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  conflictConfig = ConflictConfig {
    addConflictCommonPreconditions = Just False,
    withConflictDistractors = Just False,
    conflictDistractorAddExtraPreconditions = Nothing,
    conflictDistractorOnlyConflictLike = False,
    conflictDistractorOnlyConcurrentLike = False
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  uniqueConflictPlace = Nothing,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }

{-|
points: 0.1
generation time: 6:00min
CPU usage: 100%
-}
task2023_16 :: PickConflictConfig
task2023_16 = PickConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 2,
    flowOverall = (16, 16) ,
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  conflictConfig = ConflictConfig {
    addConflictCommonPreconditions = Just True,
    withConflictDistractors = Just True,
    conflictDistractorAddExtraPreconditions = Just True,
    conflictDistractorOnlyConflictLike = True,
    conflictDistractorOnlyConcurrentLike = False
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  uniqueConflictPlace = Just True,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 1000,
    timeout = Nothing
    }
  }

{-|
points: 0.1
generation time: 7:59min
CPU usage: 105%
-}
task2024_30 :: PickConflictConfig
task2024_30 = task2023_16

{-|
points: 0.1
generation time: 7:01min
CPU usage: 102%
-}
task2024_31 :: PickConflictConfig
task2024_31 = task2023_22
