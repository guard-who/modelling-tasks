-- |

module Modelling.PetriNet.PetriFindConflict.Config where

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

task24 :: FindConflictConfig
task24 = FindConflictConfig {
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

task26 :: FindConflictConfig
task26 = FindConflictConfig {
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
