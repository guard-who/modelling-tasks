-- |

module Modelling.PetriNet.PetriDeadlock.Config where

import Modelling.PetriNet.Reach.Deadlock (DeadlockConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.2
-}
task2023_29 :: DeadlockConfig
task2023_29 = DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 7,
  minTransitionLength = 7,
  postconditionsRange = (1, Just 2),
  preconditionsRange = (1, Just 2),
  printSolution = True,
  rejectLongerThan = Just 7,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False
  }

{-|
points: 0.25
-}
task2023_30 :: DeadlockConfig
task2023_30 = DeadlockConfig {
  numPlaces = 6,
  numTransitions = 8,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 14,
  minTransitionLength = 14,
  postconditionsRange = (1, Just 2),
  preconditionsRange = (1, Just 2),
  printSolution = True,
  rejectLongerThan = Just 14,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False
  }

{-|
points: 0.2
-}
task2024_27 :: DeadlockConfig
task2024_27 = task2023_29

{-|
points: 0.25
average generation time per instance: 1:49min
CPU usage: 99%
-}
task2024_28 :: DeadlockConfig
task2024_28 = task2023_30

{-|
points: 0.08
-}
task2024_61 :: DeadlockConfig
task2024_61 = DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  postconditionsRange = (1, Just 2),
  preconditionsRange = (1, Just 2),
  printSolution = True,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False
  }
