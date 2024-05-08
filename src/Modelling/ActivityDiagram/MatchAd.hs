{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.MatchAd (
  MatchAdConfig (..),
  MatchAdInstance (..),
  MatchAdSolution (..),
  checkMatchAdConfig,
  defaultMatchAdConfig,
  defaultMatchAdInstance,
  matchAd,
  matchAdAlloy,
  matchAdEvaluation,
  matchAdInitial,
  matchAdSolution,
  matchAdSyntax,
  matchAdTask,
 ) where

import qualified Data.Map as M (fromList, keys)

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  adConfigToAlloy,
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUMLConvConf (..),
  defaultPlantUMLConvConf,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)
import Modelling.Auxiliary.Common       (getFirstInstance)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow)
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
import Data.String.Interpolate (i, iii)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffleM)

data MatchAdInstance = MatchAdInstance {
  activityDiagram :: UMLActivityDiagram,
  plantUMLConf :: PlantUMLConvConf,
  showSolution :: Bool
} deriving (Generic, Show)

data MatchAdConfig = MatchAdConfig {
  adConfig :: AdConfig,
  maxInstances :: Maybe Integer,
  hideBranchConditions :: Bool,
  noActivityFinalInForkBlocks :: Maybe Bool,
  printSolution :: Bool
} deriving (Generic, Show)

defaultMatchAdConfig :: MatchAdConfig
defaultMatchAdConfig = MatchAdConfig {
  adConfig = defaultAdConfig,
  maxInstances = Just 50,
  hideBranchConditions = False,
  noActivityFinalInForkBlocks = Just False,
  printSolution = False
}

checkMatchAdConfig :: MatchAdConfig -> Maybe String
checkMatchAdConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkMatchAdConfig' conf

checkMatchAdConfig' :: MatchAdConfig -> Maybe String
checkMatchAdConfig' MatchAdConfig {
    adConfig,
    maxInstances,
    noActivityFinalInForkBlocks
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | noActivityFinalInForkBlocks == Just True && activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 Activity Final Node"
  | otherwise
    = Nothing

matchAdAlloy :: MatchAdConfig -> String
matchAdAlloy MatchAdConfig {
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
        Nothing -> ""

data MatchAdSolution = MatchAdSolution {
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

matchAdSolution :: MatchAdInstance -> MatchAdSolution
matchAdSolution task =
  let ad = activityDiagram task
  in MatchAdSolution {
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

matchAdTask
  :: (MonadPlantUml m, OutputMonad m)
  => FilePath
  -> MatchAdInstance
  -> LangM m
matchAdTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english [iii|
      State the names of all action nodes, the names of all object nodes,
      and the count of each other kind of element for the given activity diagram.
      |]
    german [iii|
      Geben Sie die Namen aller Aktionsknoten, die Namen aller Objektknoten,
      sowie die Anzahl jeder anderen Art von Element für
      das gegebene Aktivitätsdiagramm an.
      |]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example:|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an:|]
    code $ show matchAdInitial
    pure ()
  pure ()

matchAdInitial :: MatchAdSolution
matchAdInitial = MatchAdSolution {
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

matchAdSyntax
  :: (OutputMonad m)
  => MatchAdInstance
  -> MatchAdSolution
  -> LangM m
matchAdSyntax task sub = addPretext $ do
  let adNames = map name $ filter (\n -> isActionNode n || isObjectNode n) $ nodes $ activityDiagram task
      subNames = actionNames sub ++ objectNodeNames sub
  assertion (all (`elem` adNames) subNames) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"

matchAdEvaluation
  :: OutputMonad m
  => MatchAdInstance
  -> MatchAdSolution
  -> Rated m
matchAdEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "answer parts"
        german "Teilantworten"
      sol = matchAdSolution task
      solutionString =
        if showSolution task
        then Just $ show sol
        else Nothing
      solution = matchAdSolutionMap sol
      sub' = M.keys $ matchAdSolutionMap sub
  multipleChoice as solutionString solution sub'

matchAdSolutionMap
  :: MatchAdSolution
  -> Map (Int, Either [String] Int) Bool
matchAdSolutionMap sol =
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

matchAd
  :: (MonadAlloy m, MonadThrow m)
  => MatchAdConfig
  -> Int
  -> Int
  -> m MatchAdInstance
matchAd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchAdTask config) g

getMatchAdTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => MatchAdConfig
  -> RandT g m MatchAdInstance
getMatchAdTask config = do
  alloyInstances <- getInstances
    (maxInstances config)
    Nothing
    $ matchAdAlloy config
  randomInstances <- shuffleM alloyInstances >>= mapM parseInstance
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances >>= getFirstInstance
  return $ MatchAdInstance {
    activityDiagram = ad,
    plantUMLConf = defaultPlantUMLConvConf {
      suppressBranchConditions = hideBranchConditions config
      },
    showSolution = printSolution config
  }

defaultMatchAdInstance :: MatchAdInstance
defaultMatchAdInstance = MatchAdInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "A"},
      AdActionNode {label = 2, name = "B"},
      AdActionNode {label = 3, name = "C"},
      AdActionNode {label = 4, name = "D"},
      AdObjectNode {label = 5, name = "E"},
      AdObjectNode {label = 6, name = "F"},
      AdObjectNode {label = 7, name = "G"},
      AdObjectNode {label = 8, name = "H"},
      AdDecisionNode {label = 9},
      AdDecisionNode {label = 10},
      AdMergeNode {label = 11},
      AdMergeNode {label = 12},
      AdForkNode {label = 13},
      AdJoinNode {label = 14},
      AdActivityFinalNode {label = 15},
      AdFlowFinalNode {label = 16},
      AdInitialNode {label = 17}
    ],
    connections = [
      AdConnection {from = 1, to = 14, guard = ""},
      AdConnection {from = 2, to = 11, guard = ""},
      AdConnection {from = 3, to = 14, guard = ""},
      AdConnection {from = 4, to = 8, guard = ""},
      AdConnection {from = 5, to = 11, guard = ""},
      AdConnection {from = 6, to = 9, guard = ""},
      AdConnection {from = 7, to = 16, guard = ""},
      AdConnection {from = 8, to = 12, guard = ""},
      AdConnection {from = 9, to = 10, guard = "a"},
      AdConnection {from = 9, to = 12, guard = "b"},
      AdConnection {from = 10, to = 2, guard = "b"},
      AdConnection {from = 10, to = 5, guard = "a"},
      AdConnection {from = 11, to = 13, guard = ""},
      AdConnection {from = 12, to = 6, guard = ""},
      AdConnection {from = 13, to = 1, guard = ""},
      AdConnection {from = 13, to = 3, guard = ""},
      AdConnection {from = 13, to = 7, guard = ""},
      AdConnection {from = 14, to = 15, guard = ""},
      AdConnection {from = 17, to = 4, guard = ""}
    ]
  },
  plantUMLConf = defaultPlantUMLConvConf,
  showSolution = False
}
