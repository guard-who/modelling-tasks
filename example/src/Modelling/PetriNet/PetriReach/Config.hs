-- |

module Modelling.PetriNet.PetriReach.Config where

import Modelling.PetriNet.Reach.Reach   (ReachConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

task27 :: ReachConfig
task27 = ReachConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  postconditionsRange = (2, Just 3),
  preconditionsRange = (2, Just 2),
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True
  }

task28 :: ReachConfig
task28 = ReachConfig {
  numPlaces = 6,
  numTransitions = 6,
  capacity = Unbounded,
  drawCommands = [Circo],
  maxTransitionLength = 12,
  minTransitionLength = 12,
  postconditionsRange = (2, Just 3),
  preconditionsRange = (2, Just 2),
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True
  }
