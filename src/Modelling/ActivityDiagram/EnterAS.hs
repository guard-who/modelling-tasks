{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.EnterAS (
  EnterASInstance(..),
  EnterASConfig(..),
  EnterASSolution(..),
  defaultEnterASConfig,
  checkEnterASConfig,
  enterASAlloy,
  checkEnterASInstance,
  enterActionSequence,
  enterASTask,
  enterASInitial,
  enterASSyntax,
  enterASEvaluation,
  enterASSolution,
  enterAS,
  defaultEnterASInstance
) where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..), isActionNode, isObjectNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUMLConvConf (..),
  defaultPlantUMLConvConf,
  drawADToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith, headWithErr)
import Modelling.CdOd.Auxiliary.Util    (getInstances)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Rated,
  OutputMonad,
  ($=<<),
  english,
  german,
  translate,
  printSolutionAndAssert
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Data.Maybe (isNothing, isJust, fromJust)
import Data.String.Interpolate (i, iii)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  drawSettings :: PlantUMLConvConf,
  sampleSequence :: [String],
  showSolution :: Bool
} deriving (Generic, Show, Eq)

data EnterASConfig = EnterASConfig {
  adConfig :: ADConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  minAnswerLength :: Int,
  maxAnswerLength :: Int,
  printSolution :: Bool
} deriving (Generic, Show)

defaultEnterASConfig :: EnterASConfig
defaultEnterASConfig = EnterASConfig {
  adConfig = defaultADConfig {
    minActions = 6,
    maxActions = 8,
    minObjectNodes = 1,
    maxObjectNodes = 5,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
  minAnswerLength = 5,
  maxAnswerLength = 8,
  printSolution = False
}

checkEnterASConfig :: EnterASConfig -> Maybe String
checkEnterASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkEnterASConfig' conf

checkEnterASConfig' :: EnterASConfig -> Maybe String
checkEnterASConfig' EnterASConfig {
    adConfig,
    maxInstances,
    objectNodeOnEveryPath,
    minAnswerLength,
    maxAnswerLength
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
  | objectNodeOnEveryPath == Just True && minObjectNodes adConfig < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occurring"
  | minAnswerLength < 0
    = Just "The parameter 'minAnswerLength' should be non-negative"
  | maxAnswerLength < minAnswerLength
    = Just "The parameter 'maxAnswerLength' should be greater or equal to 'minAnswerLength'"
  | otherwise
    = Nothing

enterASAlloy :: EnterASConfig -> String
enterASAlloy EnterASConfig {
    adConfig,
    objectNodeOnEveryPath
  }
  = adConfigToAlloy modules preds adConfig
  where modules = moduleActionSequencesRules
        preds =
          [i|
            noActivityFinalNodes
            someActionNodesExistInEachBlock
            #{f objectNodeOnEveryPath "checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

checkEnterASInstance :: EnterASInstance -> Maybe String
checkEnterASInstance inst
  | suppressNodeNames (drawSettings inst)
  = Just "'suppressNodeNames' must be set to 'False' for this task type"
  | otherwise
  = Nothing

checkEnterASInstanceForConfig :: EnterASInstance -> EnterASConfig -> Maybe String
checkEnterASInstanceForConfig inst EnterASConfig {
    minAnswerLength,
    maxAnswerLength
  }
  | length solution < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | length solution > maxAnswerLength
    = Just "Solution should not be longer than parameter 'maxAnswerLength'"
  | otherwise
    = Nothing
  where solution = sampleSequence inst

newtype EnterASSolution = EnterASSolution {
  sampleSolution :: [String]
} deriving (Show, Eq)

enterActionSequence :: UMLActivityDiagram -> EnterASSolution
enterActionSequence ad =
  EnterASSolution {sampleSolution=generateActionSequence ad}

enterASTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> EnterASInstance
  -> LangM m
enterASTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< liftIO
    $ drawADToFile path (drawSettings task) $ activityDiagram task
  paragraph $ do
    translate $ do
      english [iii|
        State an action sequence for the diagram, i.e., a sequence of
        action nodes resulting in the termination of all flows of the diagram,
        by entering a list of action names.
        \n
        For example, |]
      german [iii|
        Geben Sie eine Aktionsfolge für das Diagramm an, d.h., eine Folge
        von Aktionsknoten, welche in das Terminieren aller Abläufe des Diagramms
        resultiert, indem Sie eine Liste von Aktionsnamen angeben.
        \n
        Zum Beispiel drückt |]
    code $ show enterASInitial
    translate $ do
      english [i|expresses the execution of A followed by B (under the assumption that both are action nodes).|]
      german [i|die Ausführung von A gefolgt von B aus (unter der Annahme, dass beides Aktionsknoten sind).|]
    pure ()
  pure ()

enterASInitial :: [String]
enterASInitial = ["A", "B"]

enterASSyntax
  :: (OutputMonad m)
  => EnterASInstance
  -> [String]
  -> LangM m
enterASSyntax task sub = addPretext $ do
  let adNames = map name
        $ filter (\n -> isActionNode n || isObjectNode n)
        $ nodes
        $ activityDiagram task
  assertion (all (`elem` adNames) sub) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"

enterASEvaluation
  :: (OutputMonad m)
  => EnterASInstance
  -> [String]
  -> Rated m
enterASEvaluation task sub = do
  let correct = validActionSequence sub $ activityDiagram task
      points = if correct then 1 else 0
      msolutionString =
        if showSolution task
        then Just $ show $ sampleSequence task
        else Nothing
  printSolutionAndAssert msolutionString points

enterASSolution
  :: EnterASInstance
  -> [String]
enterASSolution = sampleSequence

enterAS
  :: EnterASConfig
  -> Int
  -> Int
  -> IO EnterASInstance
enterAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getEnterASTask config) g

getEnterASTask
  :: (RandomGen g, MonadIO m)
  => EnterASConfig
  -> RandT g m EnterASInstance
getEnterASTask config = do
  instas <- liftIO $ getInstances
    (maxInstances config)
    Nothing
    $ enterASAlloy config
  rinstas <- shuffleM instas
  ad <- liftIO $ mapM (fmap snd . shuffleADNames . failWith id . parseInstance) rinstas
  let validInsta =
        headWithErr "Failed to find task instances"
        $ filter (isNothing . (`checkEnterASInstanceForConfig` config))
        $ map (\x -> EnterASInstance {
          activityDiagram=x,
          drawSettings = defaultPlantUMLConvConf {
            suppressBranchConditions = hideBranchConditions config
            },
          sampleSequence = sampleSolution $ enterActionSequence x,
          showSolution = printSolution config
        }) ad
  return validInsta

defaultEnterASInstance :: EnterASInstance
defaultEnterASInstance = EnterASInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      ADActionNode {label = 1, name = "A"},
      ADActionNode {label = 2, name = "E"},
      ADActionNode {label = 3, name = "F"},
      ADActionNode {label = 4, name = "G"},
      ADActionNode {label = 5, name = "D"},
      ADActionNode {label = 6, name = "B"},
      ADObjectNode {label = 7, name = "C"},
      ADDecisionNode {label = 8},
      ADDecisionNode {label = 9},
      ADMergeNode {label = 10},
      ADMergeNode {label = 11},
      ADForkNode {label = 12},
      ADJoinNode {label = 13},
      ADFlowFinalNode {label = 14},
      ADFlowFinalNode {label = 15},
      ADInitialNode {label = 16}
    ],
    connections = [
      ADConnection {from = 1, to = 10, guard = ""},
      ADConnection {from = 2, to = 13, guard = ""},
      ADConnection {from = 3, to = 10, guard = ""},
      ADConnection {from = 4, to = 8, guard = ""},
      ADConnection {from = 5, to = 12, guard = ""},
      ADConnection {from = 6, to = 9, guard = ""},
      ADConnection {from = 7, to = 5, guard = ""},
      ADConnection {from = 8, to = 11, guard = "a"},
      ADConnection {from = 8, to = 13, guard = "b"},
      ADConnection {from = 9, to = 1, guard = "a"},
      ADConnection {from = 9, to = 3, guard = "b"},
      ADConnection {from = 10, to = 15, guard = ""},
      ADConnection {from = 11, to = 4, guard = ""},
      ADConnection {from = 12, to = 2, guard = ""},
      ADConnection {from = 12, to = 6, guard = ""},
      ADConnection {from = 12, to = 11, guard = ""},
      ADConnection {from = 13, to = 14, guard = ""},
      ADConnection {from = 16, to = 7, guard = ""}
    ]
  },
  drawSettings = defaultPlantUMLConvConf,
  sampleSequence = ["D","E","G","B","F"],
  showSolution = False
}
