-- |

module Modelling.ActivityDiagram.SelectPetri.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
average generation time per instance: 2:30min
CPU usage: 100%
-}
task2023_37 :: SelectPetriConfig
task2023_37 = SelectPetriConfig {
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
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True,
  extraText = Nothing
  }

{-|
points: 0.15
-}
task2023_38 :: SelectPetriConfig
task2023_38 = SelectPetriConfig {
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
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True,
  extraText = Nothing
  }

{-|
points: 0.15
average generation time per instance: 3:27min
CPU usage: 110%
-}
task2024_43 :: SelectPetriConfig
task2024_43 = task2023_37

{-|
points: 0.15
average generation time per instance: 1:50min
CPU usage: 114%
-}
task2024_44 :: SelectPetriConfig
task2024_44 = SelectPetriConfig {
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
  maxInstances = Just 2000,
  hideNodeNames = True,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Dot],
  petriSvgHighlighting = True,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True,
  extraText = Nothing
  }
