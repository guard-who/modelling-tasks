{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.MatchAD (
  MatchADInstance(..),
  MatchADConfig(..),
  MatchADSolution(..),
  defaultMatchADConfig,
  checkMatchADConfig,
  matchADAlloy,
  matchADTaskDescription,
  matchADComponents,
  matchADComponentsText,
  matchADTask,
  matchADSyntax
 ) where


import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode(name),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.PlantUMLConverter (defaultPlantUMLConvConf, drawADToFile)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )
import Data.List (sort)
import Data.String.Interpolate ( i )
import Modelling.Auxiliary.Output (addPretext)

data MatchADInstance = MatchADInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data MatchADConfig = MatchADConfig {
  adConfig :: ADConfig,
  noActivityFinalInForkBlocks :: Maybe Bool
}

defaultMatchADConfig :: MatchADConfig
defaultMatchADConfig = MatchADConfig {
  adConfig = defaultADConfig,
  noActivityFinalInForkBlocks = Nothing
}

checkMatchADConfig :: MatchADConfig -> Maybe String
checkMatchADConfig conf =
  checkADConfig (adConfig conf)
  <|> checkMatchADConfig' conf

checkMatchADConfig' :: MatchADConfig -> Maybe String
checkMatchADConfig' MatchADConfig {
    adConfig,
    noActivityFinalInForkBlocks
  }
  | noActivityFinalInForkBlocks == Just True && activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 Activity Final Node"
  | otherwise
    = Nothing

matchADAlloy :: MatchADConfig -> String
matchADAlloy MatchADConfig {
    adConfig,
    noActivityFinalInForkBlocks
  }
  = adConfigToAlloy "" preds adConfig
  where
    preds =
      [i|
        #{f noActivityFinalInForkBlocks "noActivityFinalInForkBlocks"}
      |]
    f opt s =
      case opt of
        Just True -> s
        Just False -> [i| not #{s}|]
        _ -> ""

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
      soltext = [i|
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
  in (ad, soltext)

matchADTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchADInstance
  -> LangM m
matchADTask path task = do
  let (diag, _) = matchADComponents task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf diag
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ translate $ do
    english [i|State the names of all actions, the names of all object nodes, and the number
of each other type of component for the given diagram.|]
    german [i|Geben Sie die Namen aller Aktionen, die Namen aller Objektknoten, sowie die Anzahl
aller anderen Arten von Komponenten für das gegebene Aktivitätsdiagramm an.|]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example.|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an.|]
    code $ show matchADInitial

matchADInitial :: MatchADSolution
matchADInitial = MatchADSolution {
  actionNames = ["A", "B"],
  objectNodeNames = ["C", "D"],
  numberOfDecisionNodes = 2,
  numberOfMergeNodes = 2,
  numberOfForkNodes = 0,
  numberOfJoinNodes = 0,
  numberOfInitialNodes = 1,
  numberOfActivityFinalNodes = 1,
  numberOfFlowFinalNodes = 0
}

matchADSyntax
  :: (OutputMonad m)
  => MatchADInstance
  -> MatchADSolution
  -> LangM m
matchADSyntax task sub = addPretext $ do
  let (diag, _) = matchADComponents task
      adNames = map name $ filter (\n -> isActionNode n || isObjectNode n) $ nodes diag
      subNames = actionNames sub ++ objectNodeNames sub
  assertion (all (`elem` adNames) subNames) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"