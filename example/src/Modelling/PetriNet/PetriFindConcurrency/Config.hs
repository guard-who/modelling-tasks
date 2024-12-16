-- |

module Modelling.PetriNet.PetriFindConcurrency.Config where

import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  FindConcurrencyConfig (..),
  )
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
generation time per instance: 7:00min
CPU usage: 100%
-}
task2023_23 :: FindConcurrencyConfig
task2023_23 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 14),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just True,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }

{-|
points: 0.15
generation time per instance: 12:10min
CPU usage: 103%
-}
task2024_32 :: FindConcurrencyConfig
task2024_32 = task2023_23

{-|
points: 0.15
generation time per instance: 26:15min
CPU usage: 106%
-}
task2024_33 :: FindConcurrencyConfig
task2024_33 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (8, 8),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just True,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [TwoPi],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }
