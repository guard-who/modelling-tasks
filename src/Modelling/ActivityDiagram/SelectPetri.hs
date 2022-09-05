{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.SelectPetri (
  SelectPetriInstance(..),
  SelectPetriConfig(..),
  SelectPetriSolution(..),
  pickRandomLayout,
  defaultSelectPetriConfig,
  checkSelectPetriConfig,
  checkPetriInstance,
  selectPetriAlloy,
  selectPetrinet,
  selectPetriTaskDescription,
  selectPetriTask,
  selectPetriSyntax,
  selectPetriEvaluation,
  selectPetri,
  defaultSelectPetriInstance
) where

import qualified Data.Map as M (fromList, toList, keys, map, filter)
import qualified Modelling.ActivityDiagram.Petrinet as PK (PetriKey(label))

import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..), isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Isomorphism (isPetriIsomorphic)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.PlantUMLConverter (drawADToFile, defaultPlantUMLConvConf)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)

import Modelling.Auxiliary.Common (oneOf)
import Modelling.Auxiliary.Output (addPretext)
import Modelling.PetriNet.Diagram (cacheNet)
import Modelling.PetriNet.Types (PetriLike(..))

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
  singleChoice, singleChoiceSyntax
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.Except (runExceptT)
import Data.List (unfoldr, nubBy)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.GraphViz.Commands (GraphvizCommand(Dot))
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import System.Random (next)       --To be changed from 'next' to 'uniform', not possible as of now due to dependencies
import System.Random.Shuffle (shuffle', shuffleM)


data SelectPetriInstance = SelectPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  graphVizCmd :: GraphvizCommand,
  numberOfWrongNets :: Int
} deriving (Show, Eq)

data SelectPetriConfig = SelectPetriConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  petriLayout :: [GraphvizCommand],
  numberOfWrongAnswers :: Int,
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Show)

pickRandomLayout :: (MonadRandom m) => SelectPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultSelectPetriConfig :: SelectPetriConfig
defaultSelectPetriConfig = SelectPetriConfig {
  adConfig = defaultADConfig,
  maxInstances = Just 50,
  petriLayout = [Dot],
  numberOfWrongAnswers = 2,
  supportSTAbsent = Nothing,
  activityFinalsExist = Nothing,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Nothing
}

checkSelectPetriConfig :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectPetriConfig' conf

checkSelectPetriConfig' :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig' SelectPetriConfig {
    adConfig,
    supportSTAbsent,
    activityFinalsExist,
    avoidAddingSinksForFinals,
    noActivityFinalInForkBlocks
  }
  | supportSTAbsent == Just True && cycles adConfig > 0
    = Just "Setting the parameter 'supportSTAbsent' to True prohibits having more than 0 cycles"
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | noActivityFinalInForkBlocks == Just True && activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 Activity Final Node"
  | otherwise
    = Nothing

selectPetriAlloy :: SelectPetriConfig -> String
selectPetriAlloy SelectPetriConfig {
  adConfig,
  supportSTAbsent,
  activityFinalsExist,
  avoidAddingSinksForFinals,
  noActivityFinalInForkBlocks
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            #{f supportSTAbsent "supportSTAbsent"}
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
            #{f noActivityFinalInForkBlocks "noActivityFinalInForkBlocks"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

selectPetriTaskDescription :: String
selectPetriTaskDescription =
  [i|
    Look at the given Activity Diagram and the given Petri Nets, and determine which Petri Net
    corresponds to the Activity Diagram (Single Choice).
  |]

checkPetriInstance :: SelectPetriInstance -> Maybe String
checkPetriInstance inst@SelectPetriInstance {
    numberOfWrongNets
  }
  | length (wrongNets $ snd $ selectPetrinet inst) /= numberOfWrongNets
    = Just "Number of wrong nets found for given instance is unequal to numberOfWrongNets"
  | otherwise
    = Nothing

data SelectPetriSolution = SelectPetriSolution {
  matchingNet :: PetriLike PetriKey,
  wrongNets :: [PetriLike PetriKey]
} deriving (Show)

selectPetrinet :: SelectPetriInstance -> (UMLActivityDiagram, SelectPetriSolution)
selectPetrinet SelectPetriInstance {
    activityDiagram,
    seed,
    numberOfWrongNets
  } =
  let ad = snd $ shuffleADNames seed activityDiagram
      matchingNet = convertToPetrinet ad
      seeds = unfoldr (Just . next) (mkStdGen seed)
      wrongNets = take numberOfWrongNets
                  $ nubBy isPetriIsomorphic
                  $ filter (not . isPetriIsomorphic matchingNet)
                  $ map (convertToPetrinet . modifyAD ad) seeds
      solution = SelectPetriSolution {matchingNet=matchingNet, wrongNets=wrongNets}
  in (ad, solution)

modifyAD :: UMLActivityDiagram -> Int -> UMLActivityDiagram
modifyAD diag seed =
  let filteredNodes = filter (\x ->
        not (isInitialNode x) &&
        not (isActivityFinalNode x) &&
        not (isFlowFinalNode x)) $ nodes diag
      toBeModified = pickRandomItems 3 filteredNodes seed
      swappedNodes = map (\x -> if x `elem` toBeModified then swapST x else x) $ nodes diag
  in UMLActivityDiagram {nodes=swappedNodes, connections=connections diag}

-- Swap nodes translated to places to nodes translated to transitions and vice versa
swapST :: ADNode -> ADNode
swapST node =
  case node of
    ADActionNode {label, name} -> ADObjectNode {label=label, name=name}
    ADObjectNode {label, name} -> ADActionNode {label=label, name=name}
    ADDecisionNode {label} -> ADForkNode {label}
    ADForkNode {label} -> ADDecisionNode {label}
    ADMergeNode {label} -> ADJoinNode {label}
    ADJoinNode {label} -> ADMergeNode {label}
    _ -> node

pickRandomItems :: Int -> [a] -> Int -> [a]
pickRandomItems n xs seed =
  take n $ shuffle' xs (length xs) (mkStdGen seed)

selectPetriTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectPetriInstance
  -> LangM m
selectPetriTask path task = do
  let (diag, sol) = selectPetrinet task
      mapping = M.map snd $ selectPetriSolutionToMap (seed task) sol
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf diag
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  petris <- liftIO $
    traverse (\c -> runExceptT
      $ cacheNet path (show . PK.label) c False False True (graphVizCmd task)) mapping
  paragraph $ translate $ do
    english "Consider the following petrinets."
    german "Betrachten Sie die folgenden Petrinetze."
  images show id (M.map (failWith id) petris)
  paragraph $ translate $ do
    english [i|Which of these petrinets matches the given activity diagram?
Please state your answer by giving a number indicating the matching petrinet.|]
    german [i|Welcher dieser Petrinetze passt zum gegebenen Aktivitätsdiagramm?
Bitte geben Sie ihre Antwort als Zahl an, welche das passende Petrinetz repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "2"
    translate $ do
      english [i|would indicate that petrinet 2 is the matching petrinet.|]
      german  [i|würde bedeuten, dass Petrinetz 2 das passende Petrinetz ist.|]

selectPetriSolutionToMap
  :: Int
  -> SelectPetriSolution
  -> Map Int (Bool, PetriLike PetriKey)
selectPetriSolutionToMap seed sol =
  let xs = (True, matchingNet sol) : map (False, ) (wrongNets sol)
      solution = shuffle' xs (length xs) (mkStdGen seed)
  in M.fromList $ zip [1..] solution

selectPetriSyntax
  :: (OutputMonad m)
  => SelectPetriInstance
  -> Int
  -> LangM m
selectPetriSyntax task sub = addPretext $ do
  let options = M.keys $ selectPetriSolutionToMap (seed task) $ snd $ selectPetrinet task
  singleChoiceSyntax True options sub

selectPetriEvaluation
  :: OutputMonad m
  => SelectPetriInstance
  -> Int
  -> Rated m
selectPetriEvaluation task n = addPretext $ do
  let as = translations $ do
        english "petrinet"
        german "Petrinet"
      solMap = selectPetriSolutionToMap (seed task) $ snd $ selectPetrinet task
      (solution, _) = head $ M.toList $ M.map snd $ M.filter fst solMap
  singleChoice as (Just $ show solution) solution n

selectPetri
  :: SelectPetriConfig
  -> Int
  -> Int
  -> IO SelectPetriInstance
selectPetri config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectPetriTask config) g

getSelectPetriTask
  :: (RandomGen g, MonadIO m)
  => SelectPetriConfig
  -> RandT g m SelectPetriInstance
getSelectPetriTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ selectPetriAlloy config
  rinstas <- shuffleM instas
  let ad = map (failWith id . parseInstance "this" "this") rinstas
      validInsta =
        head $ filter (isNothing . checkPetriInstance)
        $ map (\x ->
          SelectPetriInstance {activityDiagram=x, seed=123, graphVizCmd=Dot, numberOfWrongNets=numberOfWrongAnswers config}) ad
  g' <- getRandom
  layout <- pickRandomLayout config
  return $ SelectPetriInstance {
    activityDiagram=activityDiagram validInsta,
    seed=g',
    graphVizCmd=layout,
    numberOfWrongNets=numberOfWrongAnswers config
  }

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

defaultSelectPetriInstance :: SelectPetriInstance
defaultSelectPetriInstance = SelectPetriInstance {
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
  seed = 5508675034223564747,
  graphVizCmd = Dot,
  numberOfWrongNets = 2
}