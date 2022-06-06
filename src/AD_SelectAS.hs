{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  SelectASSolution(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  selectActionSequence,
  selectASTaskDescription,
  selectActionSequenceText
) where

import AD_ActionSequences (generateActionSequence, validActionSequence)
import AD_Alloy (moduleActionSequencesRules)
import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Datatype (UMLActivityDiagram(..))

import Control.Applicative (Alternative ((<|>)))
import Data.List (permutations)
import Data.String.Interpolate ( i )
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')


data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  numberOfWrongSequences :: Int
} deriving (Show, Eq)

data SelectASConfig = SelectASConfig {
  adConfig :: ADConfig,
  objectNodeOnEveryPath :: Maybe Bool
} deriving (Show)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
  adConfig = defaultADConfig {
    minActions = 6,
    maxActions = 8,
    minObjectNodes = 0,
    maxObjectNodes = 0
  },
  objectNodeOnEveryPath = Nothing
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
    adConfig,
    objectNodeOnEveryPath
  }
  | objectNodeOnEveryPath == Just True && maxObjectNodes adConfig < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True requires having at least 1 ObjectNode"
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
            noActivityFinalInForkBlocks
            someActionNodesExistInEachBlock
            #{f objectNodeOnEveryPath "checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

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
        filter (not . (`validActionSequence` activityDiagram)) $
        permutations correctSequence
  in SelectASSolution {correctSequence=correctSequence, wrongSequences=wrongSequences}

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
