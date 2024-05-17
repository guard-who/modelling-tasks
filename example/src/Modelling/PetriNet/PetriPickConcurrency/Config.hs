-- |

module Modelling.PetriNet.PetriPickConcurrency.Config where

import Modelling.PetriNet.Types (
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  PickConcurrencyConfig (..),
  )
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.1
generation time: 14:00min
CPU usage: 100%
-}
task21 :: PickConcurrencyConfig
task21 = PickConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 18),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 10),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 4,
    maxTokenChangePerPlace = 2,
    flowChangeOverall = 3,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [Fdp, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    }
  }
