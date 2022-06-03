{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_MatchAD (
  MatchADInstance(..),
  MatchADConfig(..),
  MatchADSolution(..),
  defaultMatchADConfig,
  checkMatchADConfig,
  matchADAlloy,
  matchADTaskDescription,
  matchADComponents,
  matchADComponentsText
 ) where


import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Datatype (
  UMLActivityDiagram(..),
  ADNode(name),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)
import AD_Shuffle (shuffleADNames)

import Data.List (sort)
import Data.String.Interpolate ( i )


data MatchADInstance = MatchADInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

newtype MatchADConfig = MatchADConfig {
  adConfig :: ADConfig
}

defaultMatchADConfig :: MatchADConfig
defaultMatchADConfig = MatchADConfig {
  adConfig=defaultADConfig
}

checkMatchADConfig :: MatchADConfig -> Maybe String
checkMatchADConfig MatchADConfig {
  adConfig
} = checkADConfig adConfig

matchADAlloy :: MatchADConfig -> String
matchADAlloy MatchADConfig {
  adConfig
}= adConfigToAlloy "" "" adConfig

matchADTaskDescription :: String
matchADTaskDescription =
  [i|
    Look at the given Activity Diagram, and use the displayed node names as identifiers
    for the following tasks:

    a) Name all Actions of the Activity Diagram
    b) Name all Object Nodes of the Activity Diagram
    c) Enter the number of Decision Nodes in the Activity Diagram
    d) Enter the number of Merge Nodes in the Activity Diagram
    e) Enter the number of Fork Nodes in the Activity Diagram
    f) Enter the number of Join Nodes in the Activity Diagram
    g) Enter the number of Initial Nodes in the Activity Diagram
    h) Enter the number of Activity Final Nodes in the Activity Diagram
    i) Enter the number of Flow Final Nodes in the Activity Diagram
  |]

data MatchADSolution = MatchADSolution {
  actionNames :: [String],
  objectNodeNames :: [String],
  numberOfDecisionNodes :: Int,
  numberOfMergeNodes :: Int,
  numberOfForkNodes :: Int,
  numberOfJoinNodes :: Int,
  numberOfInitialNodes :: Int,
  numberOfActivityFinalNodes :: Int,
  numberOfFlowFinalNodes :: Int
} deriving (Eq, Show)

matchADComponents :: MatchADInstance -> (UMLActivityDiagram, MatchADSolution)
matchADComponents MatchADInstance {
  activityDiagram,
  seed
} =
  let ad =  snd $ shuffleADNames seed activityDiagram
      solution = MatchADSolution {
        actionNames = sort $ map name $ filter isActionNode $ nodes ad,
        objectNodeNames = sort $ map name $ filter isObjectNode $ nodes ad,
        numberOfDecisionNodes = length $ filter isDecisionNode  $ nodes ad,
        numberOfMergeNodes = length $ filter isMergeNode $ nodes ad,
        numberOfForkNodes = length $ filter isForkNode $ nodes ad,
        numberOfJoinNodes = length $ filter isJoinNode  $ nodes ad,
        numberOfInitialNodes = length $ filter isInitialNode $ nodes ad,
        numberOfActivityFinalNodes = length $ filter isActivityFinalNode $ nodes ad,
        numberOfFlowFinalNodes = length $ filter isFlowFinalNode $ nodes ad
      }
  in (ad, solution)

matchADComponentsText :: MatchADInstance -> (UMLActivityDiagram, String)
matchADComponentsText inst =
  let (ad, solution) = matchADComponents inst
      text = [i|
        Solutions for the MatchAD-Task:

        a) Names of all Actions in the Activity Diagram: #{actionNames solution}
        b) Names of all Object Nodes in the Activity Diagram: #{objectNodeNames solution}
        c) Number of Decision Nodes in the Activity Diagram: #{numberOfDecisionNodes solution}
        d) Number of Merge Nodes in the Activity Diagram: #{numberOfMergeNodes solution}
        e) Number of Fork Nodes in the Activity Diagram: #{numberOfForkNodes solution}
        f) Number of Join Nodes in the Activity Diagram: #{numberOfJoinNodes solution}
        g) Number of Initial Nodes in the Activity Diagram: #{numberOfInitialNodes solution}
        h) Number of Activity Final Nodes in the Activity Diagram: #{numberOfActivityFinalNodes solution}
        i) Number of Flow Final Nodes in the Activity Diagram: #{numberOfFlowFinalNodes solution}
      |]
  in (ad, text)
