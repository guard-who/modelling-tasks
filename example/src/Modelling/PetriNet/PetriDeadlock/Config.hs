-- |

module Modelling.PetriNet.PetriDeadlock.Config where

import Modelling.PetriNet.Reach.Deadlock (DeadlockConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.2
-}
task29 :: DeadlockConfig
task29 = DeadlockConfig {
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
  showLengthHint = True,
  showMinLengthHint = True
  }

{-|
points: 0.25
-}
task30 :: DeadlockConfig
task30 = DeadlockConfig {
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
  showLengthHint = True,
  showMinLengthHint = True
  }
