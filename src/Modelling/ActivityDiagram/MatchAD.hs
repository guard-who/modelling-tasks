{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.MatchAD (
  MatchADInstance(..),
  MatchADConfig(..),
  MatchADSolution(..),
  defaultMatchADConfig,
  checkMatchADConfig,
  matchADAlloy,
  matchADSolution,
  matchADTask,
  matchADInitial,
  matchADSyntax,
  matchADEvaluation,
  matchAD,
  defaultMatchADInstance
 ) where

import qualified Data.Map as M (fromList, keys)

import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (PlantUMLConvConf(..), defaultPlantUMLConvConf, drawADToFile)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith, headWithErr)

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
  translations,
  multipleChoice
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate ( i )
import GHC.Generics (Generic)
import Language.Alloy.Call (getInstances)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data MatchADInstance = MatchADInstance {
  activityDiagram :: UMLActivityDiagram,
  plantUMLConf :: PlantUMLConvConf
} deriving (Generic, Show)

data MatchADConfig = MatchADConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  hideBranchConditions :: Bool,
  noActivityFinalInForkBlocks :: Maybe Bool
} deriving (Generic, Show)

defaultMatchADConfig :: MatchADConfig
defaultMatchADConfig = MatchADConfig {
  adConfig = defaultADConfig,
  maxInstances = Just 50,
  hideBranchConditions = False,
  noActivityFinalInForkBlocks = Just False
}

checkMatchADConfig :: MatchADConfig -> Maybe String
checkMatchADConfig conf =
  checkADConfig (adConfig conf)
  <|> checkMatchADConfig' conf

checkMatchADConfig' :: MatchADConfig -> Maybe String
checkMatchADConfig' MatchADConfig {
    adConfig,
    maxInstances,
    noActivityFinalInForkBlocks
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
  | noActivityFinalInForkBlocks == Just True && activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 Activity Final Node"
  | otherwise
    = Nothing

matchADAlloy :: MatchADConfig -> String
matchADAlloy MatchADConfig {
    adConfig,
    noActivityFinalInForkBlocks
  }
  = adConfigToAlloy "" preds adConfig
  where
    preds =
      [i|
        #{f noActivityFinalInForkBlocks "noActivityFinalInForkBlocks"}
      |]
    f opt s =
      case opt of
        Just True -> s
        Just False -> [i| not #{s}|]
        _ -> ""

data MatchADSolution = MatchADSolution {
  actionNames :: [String],
  objectNodeNames :: [String],
  numberOfDecisionNodes :: Int,
  numberOfMergeNodes :: Int,
  numberOfForkNodes :: Int,
  numberOfJoinNodes :: Int,
  numberOfInitialNodes :: Int,
  numberOfActivityFinalNodes :: Int,
  numberOfFlowFinalNodes :: Int
} deriving (Generic, Eq, Show, Read)

matchADSolution :: MatchADInstance -> MatchADSolution
matchADSolution task =
  let ad = activityDiagram task
  in MatchADSolution {
        actionNames = sort $ map name $ filter isActionNode $ nodes ad,
        objectNodeNames = sort $ map name $ filter isObjectNode $ nodes ad,
        numberOfDecisionNodes = length $ filter isDecisionNode  $ nodes ad,
        numberOfMergeNodes = length $ filter isMergeNode $ nodes ad,
        numberOfForkNodes = length $ filter isForkNode $ nodes ad,
        numberOfJoinNodes = length $ filter isJoinNode  $ nodes ad,
        numberOfInitialNodes = length $ filter isInitialNode $ nodes ad,
        numberOfActivityFinalNodes = length $ filter isActivityFinalNode $ nodes ad,
        numberOfFlowFinalNodes = length $ filter isFlowFinalNode $ nodes ad
    }

matchADTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchADInstance
  -> LangM m
matchADTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image $=<< liftIO
    $ drawADToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english [i|State the names of all actions, the names of all object nodes, and the number
of each other type of component for the given diagram.|]
    german [i|Geben Sie die Namen aller Aktionen, die Namen aller Objektknoten, sowie die Anzahl
aller anderen Arten von Komponenten für das gegebene Aktivitätsdiagramm an.|]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example.|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an.|]
    code $ show matchADInitial
    pure ()
  pure ()

matchADInitial :: MatchADSolution
matchADInitial = MatchADSolution {
  actionNames = ["A", "B"],
  objectNodeNames = ["C", "D"],
  numberOfDecisionNodes = 2,
  numberOfMergeNodes = 2,
  numberOfForkNodes = 0,
  numberOfJoinNodes = 0,
  numberOfInitialNodes = 1,
  numberOfActivityFinalNodes = 1,
  numberOfFlowFinalNodes = 0
}

matchADSyntax
  :: (OutputMonad m)
  => MatchADInstance
  -> MatchADSolution
  -> LangM m
matchADSyntax task sub = addPretext $ do
  let adNames = map name $ filter (\n -> isActionNode n || isObjectNode n) $ nodes $ activityDiagram task
      subNames = actionNames sub ++ objectNodeNames sub
  assertion (all (`elem` adNames) subNames) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"

matchADEvaluation
  :: OutputMonad m
  => MatchADInstance
  -> MatchADSolution
  -> Rated m
matchADEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "partial answers"
        german "Teilantworten"
      sol = matchADSolution task
      solution = matchADSolutionMap sol
      sub' = M.keys $ matchADSolutionMap sub
  multipleChoice as (Just $ show sol) solution sub'

matchADSolutionMap
  :: MatchADSolution
  -> Map (Int, Either [String] Int) Bool
matchADSolutionMap sol =
  let xs = [
        Left $ sort $ actionNames sol,
        Left $ sort $ objectNodeNames sol,
        Right $ numberOfDecisionNodes sol,
        Right $ numberOfMergeNodes sol,
        Right $ numberOfForkNodes sol,
        Right $ numberOfJoinNodes sol,
        Right $ numberOfInitialNodes sol,
        Right $ numberOfActivityFinalNodes sol,
        Right $ numberOfFlowFinalNodes sol
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

matchAD
  :: MatchADConfig
  -> Int
  -> Int
  -> IO MatchADInstance
matchAD config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchADTask config) g

getMatchADTask
  :: (RandomGen g, MonadIO m)
  => MatchADConfig
  -> RandT g m MatchADInstance
getMatchADTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ matchADAlloy config
  rinstas <- shuffleM instas
  ad <- liftIO $ mapM (fmap snd . shuffleADNames . failWith id . parseInstance) rinstas
  return $ MatchADInstance {
    activityDiagram=headWithErr "Failed to find task instances" ad,
    plantUMLConf=defaultPlantUMLConvConf{suppressBranchConditions = hideBranchConditions config}
  }

defaultMatchADInstance :: MatchADInstance
defaultMatchADInstance = MatchADInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      ADActionNode {label = 1, name = "A"},
      ADActionNode {label = 2, name = "B"},
      ADActionNode {label = 3, name = "C"},
      ADActionNode {label = 4, name = "D"},
      ADObjectNode {label = 5, name = "E"},
      ADObjectNode {label = 6, name = "F"},
      ADObjectNode {label = 7, name = "G"},
      ADObjectNode {label = 8, name = "H"},
      ADDecisionNode {label = 9},
      ADDecisionNode {label = 10},
      ADMergeNode {label = 11},
      ADMergeNode {label = 12},
      ADForkNode {label = 13},
      ADJoinNode {label = 14},
      ADActivityFinalNode {label = 15},
      ADFlowFinalNode {label = 16},
      ADInitialNode {label = 17}
    ],
    connections = [
      ADConnection {from = 1, to = 14, guard = ""},
      ADConnection {from = 2, to = 11, guard = ""},
      ADConnection {from = 3, to = 14, guard = ""},
      ADConnection {from = 4, to = 8, guard = ""},
      ADConnection {from = 5, to = 11, guard = ""},
      ADConnection {from = 6, to = 9, guard = ""},
      ADConnection {from = 7, to = 16, guard = ""},
      ADConnection {from = 8, to = 12, guard = ""},
      ADConnection {from = 9, to = 10, guard = "a"},
      ADConnection {from = 9, to = 12, guard = "b"},
      ADConnection {from = 10, to = 2, guard = "b"},
      ADConnection {from = 10, to = 5, guard = "a"},
      ADConnection {from = 11, to = 13, guard = ""},
      ADConnection {from = 12, to = 6, guard = ""},
      ADConnection {from = 13, to = 1, guard = ""},
      ADConnection {from = 13, to = 3, guard = ""},
      ADConnection {from = 13, to = 7, guard = ""},
      ADConnection {from = 14, to = 15, guard = ""},
      ADConnection {from = 17, to = 4, guard = ""}
    ]
  },
  plantUMLConf = defaultPlantUMLConvConf
}
