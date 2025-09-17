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

{-|
points: 0.15
average generation time per instance: 2:30min
CPU usage: 120%
-}
task2023_19 :: MathConfig
task2023_19 = MathConfig {
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
    presenceOfSinkTransitions = Just True,
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
    },
  extraText = Nothing
  }

{-|
points: 0.15
average generation time per instance: 8:30min
CPU usage: 100%
-}
task2023_20 :: MathConfig
task2023_20 = MathConfig {
  basicConfig = BasicConfig {
    places = 5,
    transitions = 7,
    atLeastActive = 3,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (10, 10),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just True,
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
    },
  extraText = Nothing
  }

{-|
points: 0.1
average generation time per instance: 3:57min
CPU usage: 112%
-}
task2024_23 :: MathConfig
task2024_23 = task2023_19

{-|
points: 0.1
average generation time per instance: 9:22min
CPU usage: 107%
-}
task2024_24 :: MathConfig
task2024_24 = task2023_20
