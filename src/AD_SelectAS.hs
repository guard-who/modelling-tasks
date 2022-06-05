{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  selectActionSequence
) where

import AD_ActionSequences (generateActionSequence)
import AD_Alloy (moduleActionSequencesRules)
import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Datatype (UMLActivityDiagram(..))

import Control.Applicative (Alternative ((<|>)))
import Data.String.Interpolate ( i )


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

--To be implemented
selectActionSequence :: SelectASInstance -> SelectASSolution
selectActionSequence SelectASInstance {
    activityDiagram
  }
  =
  let correctSequence = generateActionSequence activityDiagram
  in SelectASSolution {correctSequence=correctSequence, wrongSequences=[]}