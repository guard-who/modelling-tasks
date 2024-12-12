-- |

module Modelling.PetriNet.PetriFindConflictPlaces.Config where

import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  ConflictConfig (..),
  GraphConfig (..),
  FindConflictConfig (..),
  )
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.2
generation time: 20:00min
CPU usage: 100%
-}
task2023_24 :: FindConflictConfig
task2023_24 = FindConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 5,
    atLeastActive = 3,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (2, 7),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Nothing,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Nothing
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 2,
    maxTokenChangePerPlace = 1,
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
    graphLayouts = [Neato],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  uniqueConflictPlace = Just True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }

{-|
points: 0.2
generation time: 20:00min
CPU usage: 100%
-}
task2023_26 :: FindConflictConfig
task2023_26 = FindConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 5,
    atLeastActive = 3,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (2, 7),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Nothing,
    presenceOfSinkTransitions = Just True,
    presenceOfSourceTransitions = Nothing
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 2,
    maxTokenChangePerPlace = 1,
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
    graphLayouts = [Neato],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  uniqueConflictPlace = Just True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }

{-|
points: 0.2
generation time: 31:14min
CPU usage: 104%
-}
task2024_34 :: FindConflictConfig
task2024_34 = task2023_24

{-|
points: 0.2
generation time: 35:16min
CPU usage: 103%
-}
task2024_35 :: FindConflictConfig
task2024_35 = task2023_26

{-|
points: 0.2
generation time: 35:16min
CPU usage: 103%
-}
task2024_36 :: FindConflictConfig
task2024_36 = FindConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 4,
    flowOverall = (16, 18),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (3, 5),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Nothing,
    presenceOfSinkTransitions = Just True,
    presenceOfSourceTransitions = Nothing
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 2,
    maxTokenChangePerPlace = 1,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  conflictConfig = ConflictConfig {
    addConflictCommonPreconditions = Just True,
    withConflictDistractors = Just True,
    conflictDistractorAddExtraPreconditions = Just True,
    conflictDistractorOnlyConflictLike = False,
    conflictDistractorOnlyConcurrentLike = True
    },
  graphConfig = GraphConfig {
    graphLayouts = [Neato],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  uniqueConflictPlace = Just True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }
