-- |

module Modelling.ActivityDiagram.MatchPetri.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchPetri (MatchPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

task39 :: MatchPetriConfig
task39 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 12,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
    },
  maxInstances = Just 10000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  supportSTAbsent = Nothing,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }

task40 :: MatchPetriConfig
task40 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 13,
    decisionMergePairs = 3,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 3,
    cycles = 3
    },
  maxInstances = Just 10000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  supportSTAbsent = Nothing,
  activityFinalsExist = Just False,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = True
  }
