{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Modelling.ActivityDiagram.FindSupportST (
  FindSupportSTInstance(..),
  FindSupportSTConfig(..),
  FindSupportSTSolution(..),
  defaultFindSupportSTConfig,
  checkFindSupportSTConfig,
  findSupportSTSolution,
  findSupportSTAlloy,
  findSupportSTTask,
  findSupportSTInitial,
  findSupportSTEvaluation,
  findSupportST,
  defaultFindSupportSTInstance
) where

import qualified Modelling.PetriNet.Types as Petri (Net (nodes))

import qualified Data.Map as M (
  filter,
  filterWithKey,
  fromList,
  keys,
  null,
  size,
  )

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..))
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (PlantUMLConvConf(..), defaultPlantUMLConvConf, drawADToFile)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith, headWithErr)

import Modelling.Auxiliary.Output (addPretext)
import Modelling.CdOd.Auxiliary.Util    (getInstances)
import Modelling.PetriNet.Types (
  Net (..),
  PetriLike (..),
  PetriNode (..),
  SimpleNode,
  isPlaceNode,
  isTransitionNode,
  )

import Control.Applicative (Alternative ((<|>)))
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
  multipleChoice
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate ( i )
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)

data FindSupportSTInstance = FindSupportSTInstance {
  activityDiagram :: UMLActivityDiagram,
  plantUMLConf :: PlantUMLConvConf,
  showSolution :: Bool
} deriving (Generic, Show)

data FindSupportSTConfig = FindSupportSTConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  hideNodeNames :: Bool,
  hideBranchConditions :: Bool,
  -- | Option to disallow activity finals to reduce semantic confusion
  activityFinalsExist :: Maybe Bool,
  -- | Avoid having to add new sink transitions for representing finals
  avoidAddingSinksForFinals :: Maybe Bool,
  printSolution :: Bool
} deriving (Generic, Show)

defaultFindSupportSTConfig :: FindSupportSTConfig
defaultFindSupportSTConfig = FindSupportSTConfig
  { adConfig = defaultADConfig,
    maxInstances = Just 50,
    hideNodeNames = False,
    hideBranchConditions = False,
    activityFinalsExist = Nothing,
    avoidAddingSinksForFinals = Nothing,
    printSolution = False
  }

checkFindSupportSTConfig :: FindSupportSTConfig -> Maybe String
checkFindSupportSTConfig conf =
  checkADConfig (adConfig conf)
  <|> findSupportSTConfig' conf

findSupportSTConfig' :: FindSupportSTConfig -> Maybe String
findSupportSTConfig' FindSupportSTConfig {
    adConfig,
    maxInstances,
    activityFinalsExist,
    avoidAddingSinksForFinals
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing

findSupportSTAlloy :: FindSupportSTConfig -> String
findSupportSTAlloy FindSupportSTConfig {
  adConfig,
  activityFinalsExist,
  avoidAddingSinksForFinals
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            not supportSTAbsent
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

data FindSupportSTSolution = FindSupportSTSolution {
  numberOfPetriNodes :: Int,
  numberOfSupportPlaces :: Int,
  numberOfSupportTransitions :: Int
} deriving (Generic, Show, Eq, Read)

findSupportSTSolution :: FindSupportSTInstance -> FindSupportSTSolution
findSupportSTSolution task =
  findSupportSTSolution' @PetriLike @SimpleNode
  $ convertToPetrinet $ activityDiagram task

findSupportSTSolution'
  :: Net p n
  => p n PetriKey
  -> FindSupportSTSolution
findSupportSTSolution' petri = FindSupportSTSolution {
    numberOfPetriNodes = M.size $ Petri.nodes petri,
    numberOfSupportPlaces = M.size $ M.filter isPlaceNode supportSTMap,
    numberOfSupportTransitions = M.size $ M.filter isTransitionNode supportSTMap
  }
  where
    supportSTMap = M.filterWithKey
      (\k _ -> isSupportST k && not (isSinkST k petri))
      $ Petri.nodes petri

isSinkST :: Net p n => PetriKey -> p n PetriKey -> Bool
isSinkST key petri = M.null $ outFlow key petri

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False

findSupportSTTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> FindSupportSTInstance
  -> LangM m
findSupportSTTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image $=<< liftIO
    $ drawADToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english [i|Translate the given activity diagram into a Petri net, then state the total count of nodes,
the count of auxiliary places and the count of auxiliary transitions of the net.|]
    german [i|Übersetzen Sie das gegebene Aktivitätsdiagramm in ein Petrinetz, geben Sie anschließend die Gesamtanzahl
an Knoten, die Anzahl der Hilfsstellen und die Anzahl der Hilfstransitionen des Netzes an.|]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example.|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an.|]
    code $ show findSupportSTInitial
    translate $ do
      english [i|In this example, the resulting net contains 10 nodes in total, with 2 auxiliary places and 3 auxiliary transitions.|]
      german [i|In diesem Beispiel enthält das entstehende Netz etwa 10 Knoten, davon 2 Hilfsstellen und 3 Hilfstransition.|]
    pure ()
  pure ()

findSupportSTInitial :: FindSupportSTSolution
findSupportSTInitial = FindSupportSTSolution {
  numberOfPetriNodes = 10,
  numberOfSupportPlaces = 2,
  numberOfSupportTransitions = 3
}

findSupportSTEvaluation
  :: OutputMonad m
  => FindSupportSTInstance
  -> FindSupportSTSolution
  -> Rated m
findSupportSTEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "answer parts"
        german "Teilantworten"
      sol = findSupportSTSolution task
      solution = findSupportSTSolutionMap sol
      sub' = M.keys $ findSupportSTSolutionMap sub
      msolutionString =
        if showSolution task
        then Just $ show sol
        else Nothing
  multipleChoice as msolutionString solution sub'

findSupportSTSolutionMap
  :: FindSupportSTSolution
  -> Map (Int, Int) Bool
findSupportSTSolutionMap sol =
  let xs = [
        numberOfPetriNodes sol,
        numberOfSupportPlaces sol,
        numberOfSupportTransitions sol
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

findSupportST
  :: FindSupportSTConfig
  -> Int
  -> Int
  -> IO FindSupportSTInstance
findSupportST config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getFindSupportSTTask config) g

getFindSupportSTTask
  :: (RandomGen g, MonadIO m)
  => FindSupportSTConfig
  -> RandT g m FindSupportSTInstance
getFindSupportSTTask config = do
  instas <- liftIO $ getInstances
    (maxInstances config)
    Nothing
    $ findSupportSTAlloy config
  rinstas <- shuffleM instas
  ad <- liftIO $ mapM (fmap snd . shuffleADNames . failWith id . parseInstance) rinstas
  return $ FindSupportSTInstance {
    activityDiagram=headWithErr "Failed to find task instances" ad,
    plantUMLConf =
      PlantUMLConvConf {
        suppressNodeNames = hideNodeNames config,
        suppressBranchConditions = hideBranchConditions config
      },
    showSolution = printSolution config
  }

defaultFindSupportSTInstance :: FindSupportSTInstance
defaultFindSupportSTInstance = FindSupportSTInstance {
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
      ADConnection {from = 4, to = 9, guard = ""},
      ADConnection {from = 5, to = 11, guard = ""},
      ADConnection {from = 6, to = 10, guard = ""},
      ADConnection {from = 7, to = 16, guard = ""},
      ADConnection {from = 8, to = 4, guard = ""},
      ADConnection {from = 9, to = 2, guard = "a"},
      ADConnection {from = 9, to = 5, guard = "b"},
      ADConnection {from = 10, to = 8, guard = "b"},
      ADConnection {from = 10, to = 12, guard = "a"},
      ADConnection {from = 11, to = 15, guard = ""},
      ADConnection {from = 12, to = 6, guard = ""},
      ADConnection {from = 13, to = 1, guard = ""},
      ADConnection {from = 13, to = 3, guard = ""},
      ADConnection {from = 13, to = 7, guard = ""},
      ADConnection {from = 14, to = 12, guard = ""},
      ADConnection {from = 17, to = 13, guard = ""}
    ]
  },
  plantUMLConf = defaultPlantUMLConvConf,
  showSolution = False
}
