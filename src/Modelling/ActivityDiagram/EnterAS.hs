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
  enterASSyntax,
  enterASEvaluation,
  enterAS,
  defaultEnterASInstance
) where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..), isActionNode, isObjectNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
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
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Data.Maybe (isNothing)
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  sampleSequence :: [String]
} deriving (Show, Eq)

data EnterASConfig = EnterASConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  minAnswerLength :: Int,
  maxAnswerLength :: Int
} deriving (Show)

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
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
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
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ do
    translate $ do
      english [i|State an action sequence for the diagram, therefore a sequence of actions resulting in
the termination of all flows of the diagram, by entering a list of action names.
For example, |]
      german [i|Geben Sie eine Aktionsfolge für das Diagramm an, d.h. eine Folge von Aktionen welche in
das Terminieren aller Abläufe des Diagramms resultiert, indem Sie eine Liste von Aktionsnamen angeben.
Zum Beispiel drückt |]
    code $ show enterASInitial
    translate $ do
      english [i|expresses the execution of A, followed by B in the diagram.|]
      german [i|die Ausführung von A, gefolgt von B im Diagramm aus.|]

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
      msolutionString = Just $ show $ sampleSequence task
  printSolutionAndAssert msolutionString points

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
  instas <- liftIO $ getInstances (maxInstances config) $ enterASAlloy config
  rinstas <- shuffleM instas
  n <- getRandom
  g' <- getRandom
  let ad = map (snd . shuffleADNames n . failWith id . parseInstance) rinstas
      validInsta =
        head $ filter (isNothing . (`checkEnterASInstance` config))
        $ map (\x -> EnterASInstance {
          activityDiagram=x,
          seed=g',
          sampleSequence=sampleSolution $ enterActionSequence x
        }) ad
  return validInsta

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

defaultEnterASInstance :: EnterASInstance
defaultEnterASInstance = EnterASInstance {
  activityDiagram = UMLActivityDiagram
      { nodes = [
        ADActionNode { label = 1, name = "A"},
        ADActionNode { label = 2, name = "B"},
        ADActionNode { label = 3, name = "C"},
        ADActionNode { label = 4, name = "D"},
        ADActionNode { label = 5, name = "E"},
        ADObjectNode { label = 6, name = "F"},
        ADDecisionNode { label = 7 },
        ADDecisionNode { label = 8 },
        ADMergeNode { label = 9 },
        ADMergeNode { label = 10 },
        ADForkNode { label = 11 },
        ADJoinNode { label = 12 },
        ADFlowFinalNode { label = 13 },
        ADFlowFinalNode { label = 14 },
        ADInitialNode { label = 15 }
      ]
    , connections = [
        ADConnection{ from = 1, to = 10, guard = ""},
        ADConnection{ from = 2, to = 14, guard = ""},
        ADConnection{ from = 3, to = 9, guard = ""},
        ADConnection{ from = 4, to = 9, guard = ""},
        ADConnection{ from = 5, to = 12, guard = ""},
        ADConnection{ from = 6, to = 1, guard = ""},
        ADConnection{ from = 7, to = 10, guard = "b"},
        ADConnection{ from = 7, to = 13, guard = "a"},
        ADConnection{ from = 8, to = 3, guard = "b"},
        ADConnection{ from = 8, to = 4, guard = "a"},
        ADConnection{ from = 9, to = 12, guard = ""},
        ADConnection{ from = 10, to = 11, guard = ""},
        ADConnection{ from = 11, to = 2, guard = ""},
        ADConnection{ from = 11, to = 5, guard = ""},
        ADConnection{ from = 11, to = 8, guard = ""},
        ADConnection{ from = 12, to = 7, guard = ""},
        ADConnection{ from = 15, to = 6, guard = ""}
      ]
    },
    seed = 7777369639206507645,
    sampleSequence = []
}