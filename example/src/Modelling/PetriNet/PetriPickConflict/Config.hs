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

task22 :: PickConflictConfig
task22 = PickConflictConfig {
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

task16 :: PickConflictConfig
task16 = PickConflictConfig {
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
