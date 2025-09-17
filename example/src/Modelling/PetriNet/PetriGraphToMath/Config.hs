{-|
Configurations might work for @PetriGraphToMath@ and @PetriMathToGraph@ tasks
-}
module Modelling.PetriNet.PetriGraphToMath.Config where

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
average generation time per instance: 4:00min
CPU usage: 110%
-}
task2023_17 :: MathConfig
task2023_17 = MathConfig {
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
    graphLayouts = [Sfdp],
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
average generation time per instance: 6:00min
CPU usage: 110%
-}
task2023_18 :: MathConfig
task2023_18 = MathConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 5,
    atLeastActive = 2,
    flowOverall = (15, 17),
    maxTokensPerPlace = 1,
    maxFlowPerEdge = 1,
    tokensOverall = (5, 5),
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
  generatedWrongInstances = 300,
  graphConfig = GraphConfig {
    graphLayouts = [Fdp],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  useDifferentGraphLayouts = False,
  wrongInstances = 3 ,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = Nothing
  }

{-|
points: 0.1
average generation time per instance: 5:16min
CPU usage: 105%
-}
task2024_21 :: MathConfig
task2024_21 = task2023_17

{-|
points: 0.1
average generation time per instance: 10:28min
CPU usage: 103%
-}
task2024_22 :: MathConfig
task2024_22 = task2023_18
