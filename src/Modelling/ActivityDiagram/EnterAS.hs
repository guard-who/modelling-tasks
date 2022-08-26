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
  enterASTaskDescription,
  enterActionSequenceText,
  enterASTask,
  enterASEvaluation
) where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..))
import Modelling.ActivityDiagram.PlantUMLConverter (drawADToFile, defaultPlantUMLConvConf)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  Rated,
  OutputMonad (..),
  english,
  german,
  translate,
  printSolutionAndAssert
  )
import Data.String.Interpolate ( i )

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show, Eq)

data EnterASConfig = EnterASConfig {
  adConfig :: ADConfig,
  objectNodeOnEveryPath :: Maybe Bool,
  minAnswerLength :: Int,
  maxAnswerLength :: Int
} deriving (Show)

defaultEnterASConfig :: EnterASConfig
defaultEnterASConfig = EnterASConfig {
  adConfig = defaultADConfig {
    minActions = 6,
    maxActions = 8,
    minObjectNodes = 0,
    maxObjectNodes = 0,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  objectNodeOnEveryPath = Nothing,
  minAnswerLength = 5,
  maxAnswerLength = 8
}

checkEnterASConfig :: EnterASConfig -> Maybe String
checkEnterASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkEnterASConfig' conf

checkEnterASConfig' :: EnterASConfig -> Maybe String
checkEnterASConfig' EnterASConfig {
    adConfig,
    objectNodeOnEveryPath,
    minAnswerLength,
    maxAnswerLength
  }
  | objectNodeOnEveryPath == Just True && minObjectNodes adConfig < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occuring"
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

checkEnterASInstance :: EnterASInstance -> EnterASConfig -> Maybe String
checkEnterASInstance inst EnterASConfig {
    minAnswerLength,
    maxAnswerLength
  }
  | length solution < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | length solution > maxAnswerLength
    = Just "Solution should not be longer than parameter 'maxAnswerLength'"
  | otherwise
    = Nothing
  where solution = sampleSolution $ snd $ enterActionSequence inst

newtype EnterASSolution = EnterASSolution {
  sampleSolution :: [String]
} deriving (Show, Eq)

enterActionSequence :: EnterASInstance -> (UMLActivityDiagram, EnterASSolution)
enterActionSequence EnterASInstance {
    activityDiagram,
    seed
  }
  =
  let ad = snd $ shuffleADNames seed activityDiagram
      solution = EnterASSolution {sampleSolution=generateActionSequence ad}
  in (ad, solution)

enterASTaskDescription :: String
enterASTaskDescription =
  [i|
    Look at the given Activity Diagram, and enter an action sequences
    resulting in all flows of the Activity Diagram being terminated.
  |]


enterActionSequenceText :: EnterASInstance -> (UMLActivityDiagram, String)
enterActionSequenceText inst =
  let (ad, solution) = enterActionSequence inst
      soltext = [i|
        Sample solution for the EnterActionSequence-Task:

        A correct Action Sequence for the given Activity Diagram is: #{sampleSolution solution}

        Other entered sequences might be checked with the function 'validActionSequence'
      |]
  in (ad, soltext)

enterASTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> EnterASInstance
  -> LangM m
enterASTask path task = do
  let (diag, _) = enterActionSequence task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf diag
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ translate $ do
    english [i|State an action sequence for the diagram, therefore a sequence of actions resulting in
the termination of all flows of the diagram, by entering a list of action names.|]
    german [i|Geben Sie eine Aktionsfolge für das Diagramm an, d.h. eine Folge von Aktionen welche in
das Terminieren aller Abläufe des Diagramms resultiert, indem Sie eine Liste von Aktionsnamen angeben.|]

enterASEvaluation
  :: (OutputMonad m)
  => EnterASInstance
  -> [String]
  -> Rated m
enterASEvaluation task sub = do
  let (diag, sol) = enterActionSequence task
      correct = validActionSequence sub diag
      points = if correct then 1 else 0
      msolutionString = Just $ show $ sampleSolution sol
  printSolutionAndAssert msolutionString points