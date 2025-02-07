-- |

module Modelling.PetriNet.PetriReach.Config where

import Modelling.PetriNet.Reach.Reach   (ReachConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.2
-}
task2023_27 :: ReachConfig
task2023_27 = ReachConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  postconditionsRange = (2, Just 3),
  preconditionsRange = (2, Just 2),
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True
  }

{-|
points: 0.25
-}
task2023_28 :: ReachConfig
task2023_28 = ReachConfig {
  numPlaces = 6,
  numTransitions = 6,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 12,
  minTransitionLength = 12,
  postconditionsRange = (2, Just 3),
  preconditionsRange = (2, Just 2),
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True
  }

{-|
points: 0.2
-}
task2024_25 :: ReachConfig
task2024_25 = task2023_27

{-|
points: 0.25
-}
task2024_26 :: ReachConfig
task2024_26 = task2023_28

{-|
points: 0.08
-}
task2024_60 :: ReachConfig
task2024_60 = ReachConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  postconditionsRange = (2, Just 3),
  preconditionsRange = (2, Just 2),
  printSolution = True,
  rejectLongerThan = Just 8,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True
  }
