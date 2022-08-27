{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  SelectASSolution(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  checkSelectASInstance,
  selectActionSequence,
  selectASTaskDescription,
  selectActionSequenceText,
  selectASTask,
  selectASEvaluation,
  selectAS
) where

import qualified Data.Map as M (fromList, toList, filter, map)
import qualified Data.Vector as V (fromList)

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..))
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
  translations,
  singleChoice
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.List (permutations, sortBy)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate ( i )
import Data.Vector.Distance (Params(..), leastChanges)
import Language.Alloy.Call (getInstances)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffle', shuffleM)


data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  numberOfWrongSequences :: Int
} deriving (Show, Eq)

data SelectASConfig = SelectASConfig {
  adConfig :: ADConfig,
  objectNodeOnEveryPath :: Maybe Bool,
  numberOfWrongAnswers :: Int,
  minAnswerLength :: Int,
  maxAnswerLength :: Int
} deriving (Show)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
  adConfig = defaultADConfig {
    minActions = 6,
    maxActions = 8,
    minObjectNodes = 0,
    maxObjectNodes = 0,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 2,
  minAnswerLength = 5,
  maxAnswerLength = 8
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
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
            _ -> ""

checkSelectASInstance :: SelectASInstance -> SelectASConfig -> Maybe String
checkSelectASInstance inst SelectASConfig {
    minAnswerLength,
    maxAnswerLength
  }
  | length solution < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | length solution > maxAnswerLength
    = Just "Solution should not be longer than parameter 'maxAnswerLength'"
  | otherwise
    = Nothing
  where solution = correctSequence $ snd $ selectActionSequence inst

data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

selectActionSequence :: SelectASInstance -> (UMLActivityDiagram, SelectASSolution)
selectActionSequence SelectASInstance {
    activityDiagram,
    seed,
    numberOfWrongSequences
  }
  =
  let ad = snd $ shuffleADNames seed activityDiagram
      correctSequence = generateActionSequence ad
      wrongSequences =
        take numberOfWrongSequences $
        sortBy (compareDistToCorrect correctSequence) $
        filter (not . (`validActionSequence` ad)) $
        permutations correctSequence
      solution = SelectASSolution {correctSequence=correctSequence, wrongSequences=wrongSequences}
  in (ad, solution)

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


selectASTaskDescription :: SelectASInstance -> String
selectASTaskDescription inst@SelectASInstance {
    seed
  }
  =
  let solution = selectActionSequence inst
      toStringList = map (foldr1 (++)) $ correctSequence (snd solution) : wrongSequences (snd solution)
      shuffledList = shuffle' toStringList (length toStringList) (mkStdGen seed)
  in
  [i|
    Look at the given Activity Diagram, and determine which of the action sequences listed below
    result in all flows of the Activity Diagram being terminated (Single Choice):

    #{listOptions shuffledList}
  |]
  where
    listOptions xs =
      unlines $ map (\(x :: String) -> [i|- #{x}|]) xs

selectActionSequenceText :: SelectASInstance -> (UMLActivityDiagram, String)
selectActionSequenceText inst =
  let (ad, solution) = selectActionSequence inst
      soltext =
        [i|
          Solution for the SelectActionSequence-Task:

          The correct Action Sequence is #{correctSequence solution}.
        |]
  in (ad, soltext)

selectASTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectASInstance
  -> LangM m
selectASTask path task = do
  let (diag, sol) = selectActionSequence task
      mapping = M.toList $ M.map snd $ selectASSolutionToMap (seed task) sol
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf diag
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ translate $ do
    english "Consider the following sequences."
    german "Betrachten Sie die folgenden Folgen."
  enumerateM (code . show) $ map (\(n,xs) -> (n, code $ show xs)) mapping
  paragraph $ translate $ do
    english [i|Which of these sequences is a valid action sequences?
Please state your answer by giving a number indicating the valid action sequence.|]
    german [i|Welcher dieser Folgen ist eine valide Aktionsfolge?
Bitte geben Sie ihre Antwort als Zahl an, welche die valide Aktionsfolge repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "2"
    translate $ do
      english [i|would indicate that sequence 2 is the valid action sequence.|]
      german  [i|würde bedeuten, dass Folge 2 die valide Aktionsfolge ist.|]

selectASSolutionToMap
  :: Int
  -> SelectASSolution
  -> Map Int (Bool, [String])
selectASSolutionToMap seed sol =
  let xs = (True, correctSequence sol) : (map (\x -> (False, x)) $ wrongSequences sol)
      solution = shuffle' xs (length xs) (mkStdGen seed)
  in M.fromList $ zip [1..] solution

selectASEvaluation
  :: OutputMonad m
  => SelectASInstance
  -> Int
  -> Rated m
selectASEvaluation task n = addPretext $ do
  let as = translations $ do
        english "action sequence"
        german "Aktionsfolge"
      solMap = selectASSolutionToMap (seed task) $ snd $ selectActionSequence task
      (solution, validAS) = head $ M.toList $ M.map snd $ M.filter fst solMap
  singleChoice as (Just $ show $ validAS) solution n

selectAS
  :: SelectASConfig
  -> Int
  -> Int
  -> IO SelectASInstance
selectAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectASTask config) g

getSelectASTask
  :: (RandomGen g, MonadIO m)
  => SelectASConfig
  -> RandT g m SelectASInstance
getSelectASTask config = do
  instas <- liftIO $ getInstances (Just 50) $ selectASAlloy config
  rinstas <- shuffleM instas
  let ad = map (failWith id . parseInstance "this" "this") rinstas
      validInsta =
        head $ filter (isNothing . (`checkSelectASInstance` config))
        $ map (\x ->
          SelectASInstance {activityDiagram=x, seed=123, numberOfWrongSequences=numberOfWrongAnswers config}) ad
  g' <- getRandom
  return $ SelectASInstance {
    activityDiagram=activityDiagram validInsta,
    seed=g',
    numberOfWrongSequences=numberOfWrongAnswers config
  }

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id