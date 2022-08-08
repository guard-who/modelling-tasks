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
  enterActionSequenceText
) where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..))
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)

import Control.Applicative (Alternative ((<|>)))
import Data.String.Interpolate ( i )

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show, Eq)

data EnterASConfig = EnterASConfig {
  adConfig :: ADConfig,
  objectNodeOnEveryPath :: Maybe Bool,
  minAnswerLength :: Int
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
  minAnswerLength = 5
}

checkEnterASConfig :: EnterASConfig -> Maybe String
checkEnterASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkEnterASConfig' conf

checkEnterASConfig' :: EnterASConfig -> Maybe String
checkEnterASConfig' EnterASConfig {
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
    minAnswerLength
  }
  | length (sampleSolution $ snd $ enterActionSequence inst) < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | otherwise
    = Nothing

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
      text = [i|
        Sample solution for the EnterActionSequence-Task:

        A correct Action Sequence for the given Activity Diagram is: #{sampleSolution solution}

        Other entered sequences might be checked with the function 'validActionSequence'
      |]
  in (ad, text)