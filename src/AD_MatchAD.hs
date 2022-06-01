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
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isActivityFinalNode, isFlowFinalNode)
import AD_Shuffle (shuffleADNames)

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
    c) Enter the number of Decision- and Merge Nodes in the Activity Diagram
    d) Enter the number of Fork- and Join Nodes in the Activity Diagram
    e) Enter the number of Activity Final Nodes in the Activity Diagram
    f) Enter the number of Flow Final Nodes in the Activity Diagram
  |]

data MatchADSolution = MatchADSolution {
  actionNames :: [String],
  objectNodeNames :: [String],
  numberOfDecisionMergeNodes :: Int,
  numberOfForkJoinNodes :: Int,
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
        actionNames = map name $ filter isActionNode $ nodes ad,
        objectNodeNames = map name $ filter isObjectNode $ nodes ad,
        numberOfDecisionMergeNodes = length $ filter (\x -> isDecisionNode x || isMergeNode x) $ nodes ad,
        numberOfForkJoinNodes = length $ filter (\x -> isForkNode x || isJoinNode x) $ nodes ad,
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
        c) Number of Decision- and Merge Nodes in the Activity Diagram: #{numberOfDecisionMergeNodes solution}
        d) Number of Fork- and Join Nodes in the Activity Diagram: #{numberOfForkJoinNodes solution}
        e) Number of Activity Final Nodes in the Activity Diagram: #{numberOfActivityFinalNodes solution}
        f) Number of Flow Final Nodes in the Activity Diagram: #{numberOfFlowFinalNodes solution}
      |]
  in (ad, text)
