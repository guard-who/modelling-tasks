-- |

module Modelling.ActivityDiagram.SelectPetri.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

task37 :: SelectPetriConfig
task37 = SelectPetriConfig {
  adConfig = AdConfig {
    minActions = 7,
    maxActions = 7,
    minObjectNodes = 3,
    maxObjectNodes = 3,
    maxNamedNodes = 10,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
    },
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  hidePetriNodeLabels = False,
  petriLayout = [Fdp],
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  supportSTAbsent = Nothing,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }

task38 :: SelectPetriConfig
task38 = SelectPetriConfig {
  adConfig = AdConfig {
    minActions = 4,
    maxActions = 4,
    minObjectNodes = 3,
    maxObjectNodes = 3,
    maxNamedNodes = 7,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
    },
  maxInstances = Just 1,
  hideNodeNames = True,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Dot],
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  supportSTAbsent = Nothing,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }
