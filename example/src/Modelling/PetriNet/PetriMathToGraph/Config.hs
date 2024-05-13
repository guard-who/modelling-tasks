{-|
Configurations might work for @PetriGraphToMath@ and @PetriMathToGraph@ tasks
-}
module Modelling.PetriNet.PetriMathToGraph.Config where

import Modelling.PetriNet.MatchToMath (
  MathConfig (..),
  )
import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  )
import Data.GraphViz.Commands           (GraphvizCommand(..))

task19 :: MathConfig
task19 = MathConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 5,
    atLeastActive = 2,
    flowOverall = (10, 12),
    maxTokensPerPlace = 1,
    maxFlowPerEdge = 1,
    tokensOverall = (2, 4),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 2,
    maxTokenChangePerPlace = 1,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  generatedWrongInstances = 300,
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Neato, Fdp, Sfdp],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  useDifferentGraphLayouts = False,
  wrongInstances = 3,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }

task20 :: MathConfig
task20 = MathConfig {
  basicConfig = BasicConfig {
    places = 5,
    transitions = 7,
    atLeastActive = 3,
    flowOverall = (13, 15),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (10, 10),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  generatedWrongInstances = 300,
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Neato, Fdp, Sfdp],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  useDifferentGraphLayouts = True,
  wrongInstances = 3,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }
