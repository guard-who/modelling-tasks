{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  SelectASSolution(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  checkSelectASInstance,
  selectActionSequence,
  selectASTask,
  selectASSyntax,
  selectASEvaluation,
  selectASSolution,
  selectAS,
  defaultSelectASInstance
) where

import qualified Data.Map as M (fromList, toList, keys, filter, map)
import qualified Data.Vector as V (fromList)

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  adConfigToAlloy,
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUMLConvConf (..),
  defaultPlantUMLConvConf,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Extra (firstJustM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  Rated,
  ($=<<),
  english,
  german,
  translate,
  translations,
  singleChoice, singleChoiceSyntax
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.List (permutations, sortBy)
import Data.Map (Map)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate          (i, iii)
import Data.Vector.Distance (Params(..), leastChanges)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  actionSequences :: Map Int (Bool, [String]),
  drawSettings :: PlantUMLConvConf,
  showSolution :: Bool
} deriving (Generic, Show, Eq)

data SelectASConfig = SelectASConfig {
  adConfig :: AdConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  numberOfWrongAnswers :: Int,
  answerLength :: !(Int, Int),
  printSolution :: Bool
} deriving (Generic, Read, Show)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
  adConfig = defaultAdConfig {
    actionLimits = (6, 8),
    objectNodeLimits = (1, 3),
    maxNamedNodes = 7,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
  numberOfWrongAnswers = 2,
  answerLength = (5, 8),
  printSolution = False
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
    adConfig,
    maxInstances,
    objectNodeOnEveryPath,
    numberOfWrongAnswers,
    answerLength
  }
  | Just instances <- maxInstances, instances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
  | numberOfWrongAnswers < 1
    = Just "The parameter 'numberOfWrongAnswers' must be set to a positive value"
  | objectNodeOnEveryPath == Just True && fst (objectNodeLimits adConfig) < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occurring"
  | fst answerLength < 0
    = Just "The parameter 'answerLength' should not contain non-negative values"
  | uncurry (>) answerLength
  = Just [iii|
    The second value of parameter 'answerLength' should be greater or equal to
    its first value.
    |]
  | otherwise
    = Nothing

selectASAlloy :: SelectASConfig -> String
selectASAlloy SelectASConfig {
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
            Nothing -> ""

checkSelectASInstance :: SelectASInstance -> Maybe String
checkSelectASInstance inst
  | suppressNodeNames (drawSettings inst)
  = Just "'suppressNodeNames' must be set to 'False' for this task type"
  | otherwise
  = Nothing

checkSelectASInstanceForConfig
  :: SelectASInstance
  -> SelectASConfig
  -> Maybe String
checkSelectASInstanceForConfig inst SelectASConfig {
  answerLength
  }
  | length solution < fst answerLength
  = Just "Solution should not be shorter than the minimal 'answerLength'"
  | length solution > snd answerLength
  = Just "Solution should not be longer than the maximal 'answerLength'"
  | otherwise
    = Nothing
  where (_, solution) = head $ M.toList $ M.map snd $ M.filter fst $ actionSequences inst

data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

selectActionSequence :: Int -> UMLActivityDiagram -> SelectASSolution
selectActionSequence numberOfWrongSequences ad =
  let correctSequence = generateActionSequence ad
      wrongSequences =
        take numberOfWrongSequences $
        sortBy (compareDistToCorrect correctSequence) $
        filter (not . (`validActionSequence` ad)) $
        permutations correctSequence
  in SelectASSolution {correctSequence=correctSequence, wrongSequences=wrongSequences}

asEditDistParams :: [String] -> Params String (String, Int, String) (Sum Int)
asEditDistParams xs = Params
    { equivalent = (==)
    , delete     = \n s    -> ("delete", n, s)
    , insert     = \n s    -> ("insert", n, s)
    , substitute = \n _ s' -> ("replace", n, s')
    , cost = \ (_, n, _) -> Sum $ abs (n - (length xs `div` 2))
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

compareDistToCorrect :: [String] -> [String] -> [String] -> Ordering
compareDistToCorrect correctSequence xs ys =
  compare (distToCorrect xs) (distToCorrect ys)
  where
    distToCorrect zs =
      getSum
      $ fst
      $ leastChanges (asEditDistParams correctSequence) (V.fromList correctSequence) (V.fromList zs)

selectASTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectASInstance
  -> LangM m
selectASTask path task = do
  let mapping = M.toList $ M.map snd $ actionSequences task
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< liftIO
    $ drawAdToFile path (drawSettings task) $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following sequences:"
    german "Betrachten Sie die folgenden Folgen:"
  enumerateM (code . show) $ map (\(n,xs) -> (n, code $ show xs)) mapping
  paragraph $ translate $ do
    english [i|Which of these sequences is a valid action sequence?
Please state your answer by giving a number indicating the valid action sequence.|]
    german [i|Welche dieser Folgen ist eine valide Aktionsfolge?
Bitte geben Sie Ihre Antwort als Zahl an, welche die valide Aktionsfolge repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel würde|]
    code "2"
    translate $ do
      english [i|
        would indicate that sequence 2 is the valid sequence of action nodes.
        |]
      german  [i|
        bedeuten, dass Folge 2 die valide Folge von Aktionsknoten ist.
        |]
    pure ()
  pure ()

selectASSolutionToMap
  :: (MonadRandom m)
  => SelectASSolution
  -> m (Map Int (Bool, [String]))
selectASSolutionToMap sol = do
  let xs = (True, correctSequence sol) : map (False, ) (wrongSequences sol)
  solution <- shuffleM xs
  return $ M.fromList $ zip [1..] solution

selectASSyntax
  :: (OutputMonad m)
  => SelectASInstance
  -> Int
  -> LangM m
selectASSyntax task sub = addPretext $ do
  let options = M.keys $ actionSequences task
  singleChoiceSyntax False options sub

selectASEvaluation
  :: OutputMonad m
  => SelectASInstance
  -> Int
  -> Rated m
selectASEvaluation task n = addPretext $ do
  let as = translations $ do
        english "action sequence"
        german "Aktionsfolge"
      solMap = actionSequences task
      (solution, validAS) = head $ M.toList $ M.map snd $ M.filter fst solMap
      solutionString =
        if showSolution task
        then Just $ show validAS
        else Nothing
  singleChoice as solutionString solution n

selectASSolution
  :: SelectASInstance
  -> Int
selectASSolution = head . M.keys . M.filter fst . actionSequences

selectAS
  :: (MonadAlloy m, MonadThrow m)
  => SelectASConfig
  -> Int
  -> Int
  -> m SelectASInstance
selectAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectASTask config) g

getSelectASTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => SelectASConfig
  -> RandT g m SelectASInstance
getSelectASTask config = do
  instas <- getInstances
    (maxInstances config)
    Nothing
    $ selectASAlloy config
  rinstas <- shuffleM instas >>= mapM parseInstance
  ad <- mapM (fmap snd . shuffleAdNames) rinstas
  validInsta <- firstJustM (\x -> do
    actionSequences <- selectASSolutionToMap $ selectActionSequence (numberOfWrongAnswers config) x
    let selectASInst = SelectASInstance {
          activityDiagram=x,
          actionSequences = actionSequences,
          drawSettings = defaultPlantUMLConvConf {
            suppressBranchConditions = hideBranchConditions config
            },
          showSolution = printSolution config
        }
    case checkSelectASInstanceForConfig selectASInst config of
      Just _ -> return Nothing
      Nothing -> return $ Just selectASInst
    ) ad
  case validInsta of
    Just x -> return x
    Nothing -> error "Failed to find task instances"

defaultSelectASInstance :: SelectASInstance
defaultSelectASInstance = SelectASInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "E"},
      AdActionNode {label = 2, name = "D"},
      AdActionNode {label = 3, name = "A"},
      AdActionNode {label = 4, name = "C"},
      AdActionNode {label = 5, name = "F"},
      AdActionNode {label = 6, name = "B"},
      AdDecisionNode {label = 7},
      AdDecisionNode {label = 8},
      AdMergeNode {label = 9},
      AdMergeNode {label = 10},
      AdForkNode {label = 11},
      AdJoinNode {label = 12},
      AdFlowFinalNode {label = 13},
      AdFlowFinalNode {label = 14},
      AdInitialNode {label = 15}
    ],
    connections = [
      AdConnection {from = 1, to = 8, guard = ""},
      AdConnection {from = 2, to = 14, guard = ""},
      AdConnection {from = 3, to = 11, guard = ""},
      AdConnection {from = 4, to = 12, guard = ""},
      AdConnection {from = 5, to = 10, guard = ""},
      AdConnection {from = 6, to = 12, guard = ""},
      AdConnection {from = 7, to = 5, guard = "c"},
      AdConnection {from = 7, to = 9, guard = "a"},
      AdConnection {from = 8, to = 9, guard = "c"},
      AdConnection {from = 8, to = 10, guard = "b"},
      AdConnection {from = 9, to = 1, guard = ""},
      AdConnection {from = 10, to = 3, guard = ""},
      AdConnection {from = 11, to = 2, guard = ""},
      AdConnection {from = 11, to = 4, guard = ""},
      AdConnection {from = 11, to = 6, guard = ""},
      AdConnection {from = 12, to = 13, guard = ""},
      AdConnection {from = 15, to = 7, guard = ""}
    ]
  },
  actionSequences = M.fromList [
    (1, (False,["F","B","A","C","D"])),
    (2, (True,["F","A","B","C","D"])),
    (3, (False,["A","F","B","C","D"]))
    ],
  drawSettings = defaultPlantUMLConvConf,
  showSolution = False
}
