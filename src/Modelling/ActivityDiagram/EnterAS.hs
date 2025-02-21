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

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (
  adConfigToAlloy,
  moduleActionSequencesRules,
  )
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  isActionNode,
  isObjectNode,
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)
import Modelling.Auxiliary.Common       (getFirstInstance)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (IndefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Rated,
  OutputCapable,
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
import Data.Maybe                       (isNothing)
import Data.String.Interpolate (i, iii)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  drawSettings :: PlantUmlConfig,
  sampleSequence :: [String],
  showSolution :: Bool
} deriving (Eq, Generic, Read, Show)

data EnterASConfig = EnterASConfig {
  adConfig :: AdConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  answerLength :: !(Int, Int),
  printSolution :: Bool
} deriving (Generic, Read, Show)

defaultEnterASConfig :: EnterASConfig
defaultEnterASConfig = EnterASConfig {
  adConfig = defaultAdConfig {
    actionLimits = (6, 8),
    objectNodeLimits = (1, 5),
    maxNamedNodes = 7,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
  answerLength = (5, 8),
  printSolution = False
}

checkEnterASConfig :: EnterASConfig -> Maybe String
checkEnterASConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkEnterASConfig' conf

checkEnterASConfig' :: EnterASConfig -> Maybe String
checkEnterASConfig' EnterASConfig {
    adConfig,
    maxInstances,
    objectNodeOnEveryPath,
    answerLength
  }
  | Just instances <- maxInstances, instances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | objectNodeOnEveryPath == Just True && fst (objectNodeLimits adConfig) < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occurring"
  | fst answerLength < 0
  = Just "The parameter 'answerLength' should not contain non-negative values"
  | uncurry (>) answerLength
  = Just [iii|
    The second value of parameter 'answerLength'
    should be greater than or equal to its first value.
    |]
  | otherwise
    = Nothing

enterASAlloy :: EnterASConfig -> String
enterASAlloy EnterASConfig {
    adConfig,
    objectNodeOnEveryPath
  }
  = adConfigToAlloy modules predicates adConfig
  where modules = moduleActionSequencesRules
        predicates =
          [i|
            noActivityFinalNodes
            someActionNodesExistInEachBlock
            #{f objectNodeOnEveryPath "checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            Nothing -> ""

checkEnterASInstance :: EnterASInstance -> Maybe String
checkEnterASInstance inst
  | suppressNodeNames (drawSettings inst)
  = Just "'suppressNodeNames' must be set to 'False' for this task type"
  | otherwise
  = Nothing

checkEnterASInstanceForConfig :: EnterASInstance -> EnterASConfig -> Maybe String
checkEnterASInstanceForConfig inst EnterASConfig {
  answerLength
  }
  | length solution < fst answerLength
  = Just [iii|
    Solution should not be shorter than
    the first value of parameter 'answerLength'.
    |]
  | length solution > snd answerLength
  = Just [iii|
    Solution should not be longer than
    the second value of parameter 'answerLength'.
    |]
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
  :: (MonadPlantUml m, OutputCapable m)
  => FilePath
  -> EnterASInstance
  -> LangM m
enterASTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (drawSettings task) $ activityDiagram task
  paragraph $ do
    translate $ do
      english [iii|
        State an action sequence (i.e., a sequence of action nodes)
        which lets all flows terminate in an execution of this diagram,
        by entering a list of action names.
        \n
        For example, |]
      german [iii|
        Geben Sie eine Aktionsfolge (d.h., eine Folge von Aktionsknoten) an,
        welche in einem Ablauf dieses Diagramms alle Flüsse terminieren lässt,
        indem Sie eine Liste von Aktionsnamen angeben.
        \n
        Zum Beispiel drückt |]
    code $ show enterASInitial
    translate $ do
      english [i|expresses the execution of A followed by B (under the assumption that both are action nodes of the diagram).|]
      german [i|die Ausführung von A gefolgt von B aus (unter der Annahme, dass beides Aktionsknoten des Diagramms sind).|]
    pure ()
  pure ()

enterASInitial :: [String]
enterASInitial = ["A", "B"]

enterASSyntax
  :: OutputCapable m
  => EnterASInstance
  -> [String]
  -> LangM m
enterASSyntax task sub = addPretext $ do
  let adNames = map name
        $ filter (\n -> isActionNode n || isObjectNode n)
        $ nodes
        $ activityDiagram task
  assertion (all (`elem` adNames) sub) $ translate $ do
    english "Referenced node names are part of the given activity diagram?"
    german "Referenzierte Knotennamen sind Bestandteil des gegebenen Aktivitätsdiagramms?"

enterASEvaluation
  :: OutputCapable m
  => EnterASInstance
  -> [String]
  -> Rated m
enterASEvaluation task sub = do
  let correct = validActionSequence sub $ activityDiagram task
      points = if correct then 1 else 0
      maybeSolutionString =
        if showSolution task
        then Just $ show $ sampleSequence task
        else Nothing
  printSolutionAndAssert IndefiniteArticle maybeSolutionString points

enterASSolution
  :: EnterASInstance
  -> [String]
enterASSolution = sampleSequence

enterAS
  :: (MonadAlloy m, MonadThrow m)
  => EnterASConfig
  -> Int
  -> Int
  -> m EnterASInstance
enterAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getEnterASTask config) g

getEnterASTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => EnterASConfig
  -> RandT g m EnterASInstance
getEnterASTask config = do
  alloyInstances <- getInstances
    (maxInstances config)
    Nothing
    $ enterASAlloy config
  randomInstances <- shuffleM alloyInstances >>= mapM parseInstance
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances
  getFirstInstance
        $ filter (isNothing . (`checkEnterASInstanceForConfig` config))
        $ map (\x -> EnterASInstance {
          activityDiagram=x,
          drawSettings = defaultPlantUmlConfig {
            suppressBranchConditions = hideBranchConditions config
            },
          sampleSequence = sampleSolution $ enterActionSequence x,
          showSolution = printSolution config
        }) ad

defaultEnterASInstance :: EnterASInstance
defaultEnterASInstance = EnterASInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "A"},
      AdActionNode {label = 2, name = "E"},
      AdActionNode {label = 3, name = "F"},
      AdActionNode {label = 4, name = "G"},
      AdActionNode {label = 5, name = "D"},
      AdActionNode {label = 6, name = "B"},
      AdObjectNode {label = 7, name = "C"},
      AdDecisionNode {label = 8},
      AdDecisionNode {label = 9},
      AdMergeNode {label = 10},
      AdMergeNode {label = 11},
      AdForkNode {label = 12},
      AdJoinNode {label = 13},
      AdFlowFinalNode {label = 14},
      AdFlowFinalNode {label = 15},
      AdInitialNode {label = 16}
    ],
    connections = [
      AdConnection {from = 1, to = 10, guard = ""},
      AdConnection {from = 2, to = 13, guard = ""},
      AdConnection {from = 3, to = 10, guard = ""},
      AdConnection {from = 4, to = 8, guard = ""},
      AdConnection {from = 5, to = 12, guard = ""},
      AdConnection {from = 6, to = 9, guard = ""},
      AdConnection {from = 7, to = 5, guard = ""},
      AdConnection {from = 8, to = 11, guard = "a"},
      AdConnection {from = 8, to = 13, guard = "b"},
      AdConnection {from = 9, to = 1, guard = "a"},
      AdConnection {from = 9, to = 3, guard = "b"},
      AdConnection {from = 10, to = 15, guard = ""},
      AdConnection {from = 11, to = 4, guard = ""},
      AdConnection {from = 12, to = 2, guard = ""},
      AdConnection {from = 12, to = 6, guard = ""},
      AdConnection {from = 12, to = 11, guard = ""},
      AdConnection {from = 13, to = 14, guard = ""},
      AdConnection {from = 16, to = 7, guard = ""}
    ]
  },
  drawSettings = defaultPlantUmlConfig,
  sampleSequence = ["D","E","G","B","F"],
  showSolution = False
}
