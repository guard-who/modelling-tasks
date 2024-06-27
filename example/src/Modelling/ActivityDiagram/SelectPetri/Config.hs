-- |

module Modelling.ActivityDiagram.SelectPetri.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
generation time: 2:30min
CPU usage: 100%
-}
task37 :: SelectPetriConfig
task37 = SelectPetriConfig {
  adConfig = AdConfig {
    actionLimits = (7, 7),
    objectNodeLimits = (3, 3),
    maxNamedNodes = 10,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  hidePetriNodeLabels = False,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  supportSTAbsent = Nothing,
  activityFinalsExist = Just False,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }

{-|
points: 0.15
-}
task38 :: SelectPetriConfig
task38 = SelectPetriConfig {
  adConfig = AdConfig {
    actionLimits = (4, 4),
    objectNodeLimits = (3, 3),
    maxNamedNodes = 7,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  maxInstances = Just 1,
  hideNodeNames = True,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Dot],
  petriSvgHighlighting = True,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  supportSTAbsent = Nothing,
  activityFinalsExist = Just False,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }
