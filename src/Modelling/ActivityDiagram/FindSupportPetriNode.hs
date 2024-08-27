{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Modelling.ActivityDiagram.FindSupportPetriNode (
  FindSupportPetriNodeConfig(..),
  FindSupportPetriNodeInstance(..),
  FindSupportPetriNodeSolution(..),
  checkFindSupportPetriNodeConfig,
  defaultFindSupportPetriNodeConfig,
  defaultFindSupportPetriNodeInstance,
  findSupportPetriNode,
  findSupportPetriNodeAlloy,
  findSupportPetriNodeEvaluation,
  findSupportPetriNodeInitial,
  findSupportPetriNodeSolution,
  findSupportPetriNodeTask,
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

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Modelling.ActivityDiagram.Alloy (
  adConfigToAlloy,
  modulePetriNet,
  )
import Modelling.ActivityDiagram.Auxiliary.Util (auxiliaryNodesAdvice)
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  )
import Modelling.ActivityDiagram.PetriNet (PetriKey(..), convertToPetriNet)
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.Auxiliary.Common       (getFirstInstance)
import Modelling.Auxiliary.Output (addPretext)
import Modelling.PetriNet.Types (
  Net (..),
  PetriLike (..),
  PetriNode (..),
  SimpleNode,
  isPlaceNode,
  isTransitionNode,
  )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  translate,
  translations,
  multipleChoice,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Map (Map)
import Data.String.Interpolate ( i )
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)

data FindSupportPetriNodeInstance = FindSupportPetriNodeInstance {
  activityDiagram :: UMLActivityDiagram,
  plantUMLConf :: PlantUmlConfig,
  showSolution :: Bool
} deriving (Generic, Show)

data FindSupportPetriNodeConfig = FindSupportPetriNodeConfig {
  adConfig :: AdConfig,
  maxInstances :: Maybe Integer,
  hideNodeNames :: Bool,
  hideBranchConditions :: Bool,
  -- | Option to disallow activity finals to reduce semantic confusion
  activityFinalsExist :: Maybe Bool,
  -- | Avoid having to add new sink transitions for representing finals
  avoidAddingSinksForFinals :: Maybe Bool,
  printSolution :: Bool
} deriving (Generic, Show)

defaultFindSupportPetriNodeConfig :: FindSupportPetriNodeConfig
defaultFindSupportPetriNodeConfig = FindSupportPetriNodeConfig
  { adConfig = defaultAdConfig {activityFinalNodes = 0, flowFinalNodes = 2},
    maxInstances = Just 50,
    hideNodeNames = False,
    hideBranchConditions = False,
    activityFinalsExist = Just False,
    avoidAddingSinksForFinals = Nothing,
    printSolution = False
  }

checkFindSupportPetriNodeConfig :: FindSupportPetriNodeConfig -> Maybe String
checkFindSupportPetriNodeConfig conf =
  checkAdConfig (adConfig conf)
  <|> findSupportPetriNodeConfig' conf

findSupportPetriNodeConfig' :: FindSupportPetriNodeConfig -> Maybe String
findSupportPetriNodeConfig' FindSupportPetriNodeConfig {
    adConfig,
    maxInstances,
    activityFinalsExist,
    avoidAddingSinksForFinals
  }
  | activityFinalNodes adConfig > 1
  = Just "There is at most one 'activityFinalNode' allowed."
  | activityFinalNodes adConfig >= 1 && flowFinalNodes adConfig >= 1
  = Just "There is no 'flowFinalNode' allowed if there is an 'activityFinalNode'."
  | Just instances <- maxInstances, instances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | Just True <- avoidAddingSinksForFinals,
    fst (actionLimits adConfig) + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing

findSupportPetriNodeAlloy :: FindSupportPetriNodeConfig -> String
findSupportPetriNodeAlloy FindSupportPetriNodeConfig {
  adConfig,
  activityFinalsExist,
  avoidAddingSinksForFinals
}
  = adConfigToAlloy modules predicates adConfig
  where modules = modulePetriNet
        predicates =
          [i|
            not supportPetriNodeAbsent
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            Nothing -> ""

data FindSupportPetriNodeSolution = FindSupportPetriNodeSolution {
  numberOfPetriNodes :: Int,
  numberOfSupportPlaces :: Int,
  numberOfSupportTransitions :: Int
} deriving (Generic, Show, Eq, Read)

findSupportPetriNodeSolution
  :: FindSupportPetriNodeInstance
  -> FindSupportPetriNodeSolution
findSupportPetriNodeSolution task =
  findSupportPetriNodeSolution' @PetriLike @SimpleNode
  $ convertToPetriNet $ activityDiagram task

findSupportPetriNodeSolution'
  :: Net p n
  => p n PetriKey
  -> FindSupportPetriNodeSolution
findSupportPetriNodeSolution' petri = FindSupportPetriNodeSolution {
    numberOfPetriNodes = M.size $ Petri.nodes petri,
    numberOfSupportPlaces = M.size $ M.filter isPlaceNode supportPetriNodeMap,
    numberOfSupportTransitions =
      M.size $ M.filter isTransitionNode supportPetriNodeMap
  }
  where
    supportPetriNodeMap = M.filterWithKey
      (\k _ -> isSupportPetriNode k && not (isSinkPetriNode k petri))
      $ Petri.nodes petri

isSinkPetriNode :: Net p n => PetriKey -> p n PetriKey -> Bool
isSinkPetriNode key petri = M.null $ outFlow key petri

isSupportPetriNode :: PetriKey -> Bool
isSupportPetriNode key =
  case key of
    SupportPetriNode {} -> True
    _ -> False

findSupportPetriNodeTask
  :: (MonadPlantUml m, OutputCapable m)
  => FilePath
  -> FindSupportPetriNodeInstance
  -> LangM m
findSupportPetriNodeTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english [i|Translate the given activity diagram into a Petri net (on paper or in your head) and then state the total count of nodes (places and transitions),
the count of auxiliary places and the count of auxiliary transitions in the net.|]
    german [i|Übersetzen Sie das gegebene Aktivitätsdiagramm in ein Petrinetz (auf dem Papier oder in Ihrem Kopf) und geben Sie dann die Gesamtanzahl
an Knoten (Stellen und Transitionen), die Anzahl der Hilfsstellen und die Anzahl der Hilfstransitionen des Netzes an.|]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example:|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an:|]
    code $ show findSupportPetriNodeInitial
    translate $ do
      english [i|In this example, the resulting net contains 10 nodes in total, of which 2 are auxiliary places and 3 are auxiliary transitions.|]
      german [i|In diesem Beispiel etwa enthält das entstehende Netz insgesamt 10 Knoten, davon 2 Hilfsstellen und 3 Hilfstransitionen.|]
    pure ()
  auxiliaryNodesAdvice True
  pure ()

findSupportPetriNodeInitial :: FindSupportPetriNodeSolution
findSupportPetriNodeInitial = FindSupportPetriNodeSolution {
  numberOfPetriNodes = 10,
  numberOfSupportPlaces = 2,
  numberOfSupportTransitions = 3
}

findSupportPetriNodeEvaluation
  :: OutputCapable m
  => FindSupportPetriNodeInstance
  -> FindSupportPetriNodeSolution
  -> Rated m
findSupportPetriNodeEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "answer parts"
        german "Teilantworten"
      sol = findSupportPetriNodeSolution task
      solution = findSupportPetriNodeSolutionMap sol
      sub' = M.keys $ findSupportPetriNodeSolutionMap sub
      maybeSolutionString =
        if showSolution task
        then Just $ show sol
        else Nothing
  multipleChoice DefiniteArticle as maybeSolutionString solution sub'

findSupportPetriNodeSolutionMap
  :: FindSupportPetriNodeSolution
  -> Map (Int, Int) Bool
findSupportPetriNodeSolutionMap sol =
  let xs = [
        numberOfPetriNodes sol,
        numberOfSupportPlaces sol,
        numberOfSupportTransitions sol
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

findSupportPetriNode
  :: (MonadAlloy m, MonadThrow m)
  => FindSupportPetriNodeConfig
  -> Int
  -> Int
  -> m FindSupportPetriNodeInstance
findSupportPetriNode config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getFindSupportPetriNodeTask config) g

getFindSupportPetriNodeTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => FindSupportPetriNodeConfig
  -> RandT g m FindSupportPetriNodeInstance
getFindSupportPetriNodeTask config = do
  alloyInstances <- getInstances
    (maxInstances config)
    Nothing
    $ findSupportPetriNodeAlloy config
  randomInstances <- shuffleM alloyInstances >>= mapM parseInstance
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances >>= getFirstInstance
  return $ FindSupportPetriNodeInstance {
    activityDiagram = ad,
    plantUMLConf =
      PlantUmlConfig {
        suppressNodeNames = hideNodeNames config,
        suppressBranchConditions = hideBranchConditions config
      },
    showSolution = printSolution config
  }

defaultFindSupportPetriNodeInstance :: FindSupportPetriNodeInstance
defaultFindSupportPetriNodeInstance = FindSupportPetriNodeInstance {
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
      AdConnection {from = 4, to = 9, guard = ""},
      AdConnection {from = 5, to = 11, guard = ""},
      AdConnection {from = 6, to = 10, guard = ""},
      AdConnection {from = 7, to = 16, guard = ""},
      AdConnection {from = 8, to = 4, guard = ""},
      AdConnection {from = 9, to = 2, guard = "a"},
      AdConnection {from = 9, to = 5, guard = "b"},
      AdConnection {from = 10, to = 8, guard = "b"},
      AdConnection {from = 10, to = 12, guard = "a"},
      AdConnection {from = 11, to = 15, guard = ""},
      AdConnection {from = 12, to = 6, guard = ""},
      AdConnection {from = 13, to = 1, guard = ""},
      AdConnection {from = 13, to = 3, guard = ""},
      AdConnection {from = 13, to = 7, guard = ""},
      AdConnection {from = 14, to = 12, guard = ""},
      AdConnection {from = 17, to = 13, guard = ""}
    ]
  },
  plantUMLConf = defaultPlantUmlConfig,
  showSolution = False
}
