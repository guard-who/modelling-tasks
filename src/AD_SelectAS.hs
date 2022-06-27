{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  SelectASSolution(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  checkSelectASInstance,
  selectActionSequence,
  selectASTaskDescription,
  selectActionSequenceText
) where

import qualified Data.Vector as V (fromList)

import AD_ActionSequences (generateActionSequence, validActionSequence)
import AD_Alloy (moduleActionSequencesRules)
import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Datatype (UMLActivityDiagram(..))

import Control.Applicative (Alternative ((<|>)))
import Data.List (permutations, sortBy)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate ( i )
import Data.Vector.Distance (Params(..), leastChanges)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')


data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  numberOfWrongSequences :: Int
} deriving (Show, Eq)

data SelectASConfig = SelectASConfig {
  adConfig :: ADConfig,
  objectNodeOnEveryPath :: Maybe Bool,
  minAnswerLength :: Int
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
  minAnswerLength = 5
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
    adConfig,
    objectNodeOnEveryPath,
    minAnswerLength
  }
  | objectNodeOnEveryPath == Just True && minObjectNodes adConfig < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occuring"
  | minAnswerLength < 0
    = Just "The parameter 'minAnswerLength' should be non-negative"
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
    minAnswerLength
  }
  | length (correctSequence $ selectActionSequence inst) < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | otherwise
    = Nothing

data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

selectActionSequence :: SelectASInstance -> SelectASSolution
selectActionSequence SelectASInstance {
    activityDiagram,
    numberOfWrongSequences
  }
  =
  let correctSequence = generateActionSequence activityDiagram
      wrongSequences =
        take numberOfWrongSequences $
        sortBy (compareDistToCorrect correctSequence) $
        filter (not . (`validActionSequence` activityDiagram)) $
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


selectASTaskDescription :: SelectASInstance -> String
selectASTaskDescription inst@SelectASInstance {
    seed
  }
  =
  let solution = selectActionSequence inst
      toStringList = map (foldr1 (++)) (correctSequence solution : wrongSequences solution)
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

selectActionSequenceText :: SelectASInstance -> String
selectActionSequenceText inst =
  let solution = correctSequence $ selectActionSequence inst
  in
  [i|
    Solution for the SelectActionSequence-Task:

    The correct Action Sequence is #{solution}.
  |]
