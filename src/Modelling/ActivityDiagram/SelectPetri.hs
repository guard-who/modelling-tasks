{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Modelling.ActivityDiagram.SelectPetri (
  SelectPetriInstance(..),
  SelectPetriConfig(..),
  SelectPetriSolution(..),
  defaultSelectPetriConfig,
  checkSelectPetriConfig,
  checkPetriInstance,
  selectPetriAlloy,
  selectPetriNet,
  selectPetriTask,
  selectPetriSyntax,
  selectPetriEvaluation,
  selectPetriSolution,
  selectPetri,
  defaultSelectPetriInstance
  ) where

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Capabilities.PlantUml            (MonadPlantUml)
import qualified Data.Map as M (empty, size, fromList, toList, keys, map, filter)
import qualified Modelling.ActivityDiagram.Datatype as Ad (AdNode(label))
import qualified Modelling.ActivityDiagram.PetriNet as PK (PetriKey (label))

import Modelling.ActivityDiagram.Alloy  (adConfigToAlloy, modulePetriNet)
import Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice,
  weightedShuffle,
  )
import qualified Modelling.ActivityDiagram.Config as Config (
  AdConfig(activityFinalNodes,flowFinalNodes),
  )
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, cycles, forkJoinPairs),
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  isActivityFinalNode,
  isFlowFinalNode,
  isInitialNode,
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Isomorphism (isPetriIsomorphic)
import Modelling.ActivityDiagram.PetriNet (PetriKey (..), convertToPetriNet)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames, shufflePetri)

import Modelling.Auxiliary.Common (
  TaskGenerationException (NoInstanceAvailable),
  oneOf,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  extra,
  )
import Modelling.PetriNet.Diagram (cacheNet)
import Modelling.PetriNet.Types (
  DrawSettings (..),
  PetriLike (..),
  SimpleNode (..),
  SimplePetriLike,
  )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Control.Monad.Catch              (MonadThrow, throwM)
import Control.Monad.Extra (loopM, firstJustM)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  ($=<<),
  english,
  german,
  reRefuseLangM,
  translate,
  translations,
  singleChoice,
  singleChoiceSyntax,
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Bifunctor (second)
import Data.List (find, genericLength)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.Graph.Inductive (Gr, mkGraph, lab, level)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.String.Interpolate          (i, iii)
import Data.Traversable                 (for)
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)
import Modelling.ActivityDiagram.MatchPetri (
  MatchPetriSolution (..),
  mapTypesToLabels,
  )


data SelectPetriInstance = SelectPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  plantUMLConf :: PlantUmlConfig,
  petriDrawConf :: DrawSettings,
  petriNets :: Map Int (Bool, SimplePetriLike PetriKey),
  showSolution :: Bool,
  addText :: Maybe (Map Language String)
} deriving (Generic, Show)

data SelectPetriConfig = SelectPetriConfig {
  adConfig :: AdConfig,
  maxInstances :: Maybe Integer,
  hideNodeNames :: Bool,
  hideBranchConditions :: Bool,
  hidePetriNodeLabels :: Bool,
  petriLayout :: [GraphvizCommand],
  -- | Whether highlighting on hover should be enabled
  petriSvgHighlighting :: Bool,
  numberOfWrongAnswers :: Int,
  numberOfModifications :: Int,
  modifyAtMid :: Bool,
  -- | Option to prevent auxiliary PetriNodes from occurring
  auxiliaryPetriNodeAbsent :: Maybe Bool,
  -- | Force presence or absence of new sink transitions for representing finals
  presenceOfSinkTransitionsForFinals :: Maybe Bool,
  -- | Avoid Activity Finals in concurrent flows to reduce confusion
  noActivityFinalInForkBlocks :: Maybe Bool,
  printSolution :: Bool,
  extraText :: Maybe (Map Language String)
} deriving (Generic, Show)

pickRandomLayout :: (MonadRandom m) => SelectPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultSelectPetriConfig :: SelectPetriConfig
defaultSelectPetriConfig = SelectPetriConfig {
  adConfig = defaultAdConfig
    { Config.activityFinalNodes = 0
    , Config.flowFinalNodes = 2
    },
  maxInstances = Just 50,
  hideNodeNames = False,
  hideBranchConditions = False,
  hidePetriNodeLabels = False,
  petriLayout = [Dot],
  petriSvgHighlighting = True,
  numberOfWrongAnswers = 2,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True,
  printSolution = False,
  extraText = Nothing
}

checkSelectPetriConfig :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkSelectPetriConfig' conf

checkSelectPetriConfig' :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig' SelectPetriConfig {
    adConfig,
    maxInstances,
    petriLayout,
    numberOfWrongAnswers,
    numberOfModifications,
    auxiliaryPetriNodeAbsent,
    presenceOfSinkTransitionsForFinals,
    noActivityFinalInForkBlocks
  }
  | Config.activityFinalNodes adConfig > 1
  = Just "There is at most one 'activityFinalNode' allowed."
  | Config.activityFinalNodes adConfig >= 1 && Config.flowFinalNodes adConfig >= 1
  = Just "There is no 'flowFinalNode' allowed if there is an 'activityFinalNode'."
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | numberOfWrongAnswers < 1
    = Just "The parameter 'numberOfWrongAnswers' must be set to a positive value"
  | numberOfModifications < 1
    = Just "The parameter 'numberOfModifications' must be set to a positive value"
  | auxiliaryPetriNodeAbsent == Just True && cycles adConfig > 0
  = Just [iii|
    Setting the parameter 'auxiliaryPetriNodeAbsent' to True
    prohibits having more than 0 cycles
    |]
  | Just False <- presenceOfSinkTransitionsForFinals,
    fst (actionLimits adConfig) + forkJoinPairs adConfig < 1
    = Just "The option 'presenceOfSinkTransitionsForFinals = Just False' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | noActivityFinalInForkBlocks == Just True && Config.activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 'activityFinalNodes'"
  | noActivityFinalInForkBlocks == Just False && Config.activityFinalNodes adConfig == 0
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to False implies that there are 'activityFinalNodes'"
  | null petriLayout
    = Just "The parameter 'petriLayout' can not be the empty list"
  | any (`notElem` [Dot, Neato, TwoPi, Circo, Fdp]) petriLayout
    = Just "The parameter 'petriLayout' can only contain the options Dot, Neato, TwoPi, Circo and Fdp"
  | otherwise
    = Nothing

selectPetriAlloy :: SelectPetriConfig -> String
selectPetriAlloy SelectPetriConfig {
  adConfig,
  auxiliaryPetriNodeAbsent,
  presenceOfSinkTransitionsForFinals,
  noActivityFinalInForkBlocks
}
  = adConfigToAlloy modules predicates adConfig
  where
    activityFinalsExist = Just (Config.activityFinalNodes adConfig > 0)
    modules = modulePetriNet
    predicates =
          [i|
            #{f auxiliaryPetriNodeAbsent "auxiliaryPetriNodeAbsent"}
            #{f activityFinalsExist "activityFinalsExist"}
            #{f (not <$> presenceOfSinkTransitionsForFinals) "avoidAddingSinksForFinals"}
            #{f noActivityFinalInForkBlocks "noActivityFinalInForkBlocks"}
          |]
    f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            Nothing -> ""

checkPetriInstance :: SelectPetriInstance -> SelectPetriConfig -> Maybe String
checkPetriInstance inst SelectPetriConfig {
    numberOfWrongAnswers
  }
  | M.size (M.filter (not . fst) $ petriNets inst) /= numberOfWrongAnswers
    = Just "Number of wrong nets found for given instance is unequal to numberOfWrongAnswers"
  | otherwise
    = Nothing

data SelectPetriSolution = SelectPetriSolution {
  matchingNet :: SimplePetriLike PetriKey,
  wrongNets :: [SimplePetriLike PetriKey]
} deriving (Show)

selectPetriNet
  :: (MonadRandom m)
  => Int
  -> Int
  -> Bool
  -> UMLActivityDiagram
  -> m SelectPetriSolution
selectPetriNet numberOfWrongNets numberOfModifications modifyAtMid ad = do
  let matchingNet = convertToPetriNet ad
  wrongNets <- loopM (\xs -> do
      modAd <- modifyAd ad numberOfModifications modifyAtMid
      let petri = convertToPetriNet modAd
      if any (isPetriIsomorphic petri) (matchingNet:xs)
        then return $ Left xs
      else
        if length (petri:xs) < numberOfWrongNets
          then return $ Left (petri:xs)
        else return $ Right (petri:xs)
    ) []
  return SelectPetriSolution {
    matchingNet = matchingNet,
    wrongNets = wrongNets
  }

modifyAd
  :: (MonadRandom m)
  => UMLActivityDiagram
  -> Int
  -> Bool
  -> m UMLActivityDiagram
modifyAd diag numberOfModifications modifyAtMid = do
  let ns = distToStartNode diag
      filteredNodes = filter (\(x,_) ->
        not (isInitialNode x) &&
        not (isActivityFinalNode x) &&
        not (isFlowFinalNode x)) ns
      weightFunc =
        if modifyAtMid then weightBySquaredDev filteredNodes
        else const (1.0 :: Double)
      weightedNodes = map (second weightFunc) filteredNodes
  shuffledNodes <- weightedShuffle weightedNodes
  let toBeModified = take numberOfModifications $ reverse shuffledNodes
      swappedNodes = map
        (\x -> if x `elem` toBeModified then swapPetriNode x else x)
        $ nodes diag
  return UMLActivityDiagram {nodes=swappedNodes, connections=connections diag}

-- Swap nodes translated to places to nodes translated to transitions and vice versa
swapPetriNode :: AdNode -> AdNode
swapPetriNode node =
  case node of
    AdActionNode {label, name} -> AdObjectNode {label=label, name=name}
    AdObjectNode {label, name} -> AdActionNode {label=label, name=name}
    AdDecisionNode {label} -> AdForkNode {label}
    AdForkNode {label} -> AdDecisionNode {label}
    AdMergeNode {label} -> AdJoinNode {label}
    AdJoinNode {label} -> AdMergeNode {label}
    _ -> node

distToStartNode :: UMLActivityDiagram -> [(AdNode, Int)]
distToStartNode diag =
  let startNode = head $ map Ad.label $ filter isInitialNode $ nodes diag
      grNodes = map (\x -> (Ad.label x, x)) $ nodes diag
      grEdges = map (\x -> (from x, to x, guard x)) $ connections diag
      graph = mkGraph grNodes grEdges :: Gr AdNode String
  in map (\(x,y) -> (fromJust $ lab graph x, y)) $ level startNode graph

weightBySquaredDev :: (Real w) => [(a, w)] -> w -> Double
weightBySquaredDev xs w = square (realToFrac w - avg) + epsilon
  where
    epsilon = 0.1 -- Small constant to avoid 0-weights
    square x = x * x
    avg = realToFrac (sum (map snd xs)) / genericLength xs

selectPetriTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadPlantUml m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> SelectPetriInstance
  -> LangM m
selectPetriTask path task = do
  let mapping = M.map snd $ petriNets task
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (plantUMLConf task) $ activityDiagram task
  let drawSetting = petriDrawConf task
  paragraph $ translate $ do
    english "Consider the following Petri nets:"
    german "Betrachten Sie die folgenden Petrinetze:"
  images show id
    $=<< for
      mapping
      (\c -> cacheNet path (show . PK.label) c drawSetting)
  paragraph $ translate $ do
    english [i|Which of these Petri nets is the translation of the given activity diagram?
Please state your answer by giving a number indicating the matching Petri net.|]
    german [i|Welches dieser Petrinetze ist die Übersetzung des gegebenen Aktivitätsdiagramms?
Bitte geben Sie Ihre Antwort als Zahl an, welche das passende Petrinetz repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel würde|]
    code "2"
    translate $ do
      english [i|would indicate that Petri net 2 is the matching Petri net.|]
      german  [i|bedeuten, dass Petrinetz 2 das passende Petrinetz ist.|]
    pure ()
  finalNodesAdvice False

  extra $ addText task

  pure ()

selectPetriSolutionToMap
  :: (MonadRandom m)
  => SelectPetriSolution
  -> m (Map Int (Bool, SimplePetriLike PetriKey))
selectPetriSolutionToMap sol = do
  let xs = (True, matchingNet sol) : map (False, ) (wrongNets sol)
  solution <- shuffleM xs
  return $ M.fromList $ zip [1..] solution

selectPetriSyntax
  :: OutputCapable m
  => SelectPetriInstance
  -> Int
  -> LangM m
selectPetriSyntax task sub = addPretext $ do
  let options = M.keys $ petriNets task
  singleChoiceSyntax False options sub

selectPetriEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadPlantUml m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> SelectPetriInstance
  -> Int
  -> LangM m
selectPetriEvaluation path task n = addPretext $ do
  let as = translations $ do
        english "Petri net"
        german "Petrinetz"
      solMap = petriNets task
      (solution, _) = head $ M.toList $ M.map snd $ M.filter fst solMap
      maybeSolutionString =
        if showSolution task
        then Just $ show solution
        else Nothing
  reRefuseLangM (singleChoice DefiniteArticle as maybeSolutionString solution n) $ do
    when (showSolution task) $ do

      when (suppressNodeNames $ plantUMLConf task) $ paragraph $ do
        translate $ do
          english "The original activity diagram with node names looks like this:"
          german "Das originale Aktivitätsdiagramm sieht mit Knotennamen wie folgt aus:"

        let alteredConfig = (plantUMLConf task) { suppressNodeNames = False }
        image $=<< drawAdToFile path alteredConfig $ activityDiagram task
        pure ()

      let (_, correctNet) = fromJust $ find fst $ petriNets task
      unless (withPlaceNames $ petriDrawConf task) $ paragraph $ do
        translate $ do
          english "The translated Petri net (including node names) looks like this:"
          german "Das aus dem Aktivitätsdiagramm übersetze Petrinetz sieht mit Knotennamen wie folgt aus:"

        let drawSetting = (petriDrawConf task)
              { withPlaceNames = True
              , withTransitionNames = True
              }
        image $=<< cacheNet path (show . PK.label) correctNet drawSetting
        pure ()

      paragraph $ translate $ do
        english "The mapping of the nodes from the activity diagram to nodes from the Petri net is as follows:"
        german "Die Zuordnung der Knoten aus dem Aktivitätsdiagramm zu Knoten aus dem Petrinetz sieht wie folgt aus:"


      let MatchPetriSolution{..} = mapTypesToLabels correctNet

      paragraph $ do
        translate $ do
          english "Action nodes:"
          german "Aktionsknoten:"
        code $ show actionNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Activity final nodes:"
          german "Aktivitätsenden:"
        code $ show activityFinalNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Object nodes:"
          german "Objektknoten:"
        code $ show objectNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Decision nodes:"
          german "Verzweigungsknoten:"
        code $ show decisionNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Flow final nodes:"
          german "Flussenden:"
        code $ show flowFinalNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Merge nodes:"
          german "Verbindungsknoten:"
        code $ show mergeNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Forks:"
          german "Gabelungen:"
        code $ show forks
        pure ()

      paragraph $ do
        translate $ do
          english "Joins:"
          german "Vereinigungen:"
        code $ show joins
        pure ()

      paragraph $ do
        translate $ do
          english "Initial nodes:"
          german "Startknoten:"
        code $ show initialNodes
        pure ()

      paragraph $ do
        translate $ do
          english "Auxiliary places / transitions:"
          german "Hilfsstellen und -transitionen:"
        code $ show auxiliaryPetriNodes
        pure ()

      pure ()

selectPetriSolution
  :: SelectPetriInstance
  -> Int
selectPetriSolution = head . M.keys . M.filter fst . petriNets

selectPetri
  :: (MonadAlloy m, MonadThrow m)
  => SelectPetriConfig
  -> Int
  -> Int
  -> m SelectPetriInstance
selectPetri config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectPetriTask config) g

getSelectPetriTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => SelectPetriConfig
  -> RandT g m SelectPetriInstance
getSelectPetriTask config = do
  instances <- getInstances
    (maxInstances config)
    Nothing
    $ selectPetriAlloy config
  randomInstances <- shuffleM instances >>= mapM parseInstance
  layout <- pickRandomLayout config
  let plantUMLConf = PlantUmlConfig {
        suppressNodeNames = hideNodeNames config,
        suppressBranchConditions = hideBranchConditions config
      }
      petriDrawConf = DrawSettings {
        withPlaceNames = not $ hidePetriNodeLabels config,
        withSvgHighlighting = petriSvgHighlighting config,
        withTransitionNames = not $ hidePetriNodeLabels config,
        with1Weights = False,
        withGraphvizCommand = layout
      }
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances
  validInstances <- firstJustM (\x -> do
    sol <- selectPetriNet
      (numberOfWrongAnswers config)
      (numberOfModifications config)
      (modifyAtMid config)
      x
    p <- fmap snd $ shufflePetri $ matchingNet sol
    ps <- mapM (fmap snd . shufflePetri) $ wrongNets sol
    petriNets <- selectPetriSolutionToMap
      $ SelectPetriSolution {matchingNet=p, wrongNets=ps}
    let petriInst = SelectPetriInstance {
          activityDiagram=x,
          plantUMLConf=plantUMLConf,
          petriDrawConf=petriDrawConf,
          petriNets = petriNets,
          showSolution = printSolution config,
          addText = extraText config
        }
    case checkPetriInstance petriInst config of
      Just _ -> return Nothing
      Nothing -> return $ Just petriInst
    ) ad
  case validInstances of
    Just x -> return x
    Nothing -> throwM NoInstanceAvailable

defaultSelectPetriInstance :: SelectPetriInstance
defaultSelectPetriInstance =  SelectPetriInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "A"},
      AdActionNode {label = 2, name = "B"},
      AdActionNode {label = 3, name = "E"},
      AdActionNode {label = 4, name = "G"},
      AdObjectNode {label = 5, name = "D"},
      AdObjectNode {label = 6, name = "C"},
      AdObjectNode {label = 7, name = "F"},
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
      AdConnection {from = 1, to = 12, guard = ""},
      AdConnection {from = 2, to = 12, guard = ""},
      AdConnection {from = 3, to = 10, guard = ""},
      AdConnection {from = 4, to = 15, guard = ""},
      AdConnection {from = 5, to = 14, guard = ""},
      AdConnection {from = 6, to = 14, guard = ""},
      AdConnection {from = 7, to = 13, guard = ""},
      AdConnection {from = 8, to = 4, guard = ""},
      AdConnection {from = 9, to = 1, guard = "b"},
      AdConnection {from = 9, to = 2, guard = "a"},
      AdConnection {from = 10, to = 9, guard = "b"},
      AdConnection {from = 10, to = 11, guard = "a"},
      AdConnection {from = 11, to = 3, guard = ""},
      AdConnection {from = 12, to = 8, guard = ""},
      AdConnection {from = 13, to = 5, guard = ""},
      AdConnection {from = 13, to = 6, guard = ""},
      AdConnection {from = 13, to = 11, guard = ""},
      AdConnection {from = 14, to = 16, guard = ""},
      AdConnection {from = 17, to = 7, guard = ""}
    ]
  },
  plantUMLConf = defaultPlantUmlConfig,
  petriDrawConf = DrawSettings {
    withPlaceNames = True,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Dot
  },
  petriNets = M.fromList [
    (1,(False, PetriLike {
      allNodes = M.fromList [
        (NormalPetriNode {label = 1, sourceNode = AdActionNode {label = 2, name = "B"}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 9},1)]}),
        (AuxiliaryPetriNode {label = 2},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 24, sourceNode = AdJoinNode {label = 12}},1)]}),
        (NormalPetriNode {label = 3, sourceNode = AdActionNode {label = 8, name = "H"}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 18},1)]}),
        (NormalPetriNode {label = 4, sourceNode = AdDecisionNode {label = 10}},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 6},1),(AuxiliaryPetriNode {label = 23},1)]}),
        (AuxiliaryPetriNode {label = 5},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 7, sourceNode = AdActionNode {label = 6, name = "C"}},1)]}),
        (AuxiliaryPetriNode {label = 6},
        SimpleTransition {
          flowOut = M.fromList [(NormalPetriNode {label = 20, sourceNode = AdDecisionNode {label = 9}},1)]}),
        (NormalPetriNode {label = 7, sourceNode = AdActionNode {label = 6, name = "C"}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 12},1)]}),
        (NormalPetriNode {label = 8, sourceNode = AdActionNode {label = 3, name = "E"}},
        SimpleTransition {
          flowOut = M.fromList [(NormalPetriNode {label = 4, sourceNode = AdDecisionNode {label = 10}},1)]}),
        (AuxiliaryPetriNode {label = 9},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 24, sourceNode = AdJoinNode {label = 12}},1)]}),
        (AuxiliaryPetriNode {label = 10},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 3, sourceNode = AdActionNode {label = 8, name = "H"}},1)]}),
        (NormalPetriNode {label = 11, sourceNode = AdMergeNode {label = 11}},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 8, sourceNode = AdActionNode {label = 3, name = "E"}},1)]}),
        (AuxiliaryPetriNode {label = 12},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 14, sourceNode = AdJoinNode {label = 14}},1)]}),
        (NormalPetriNode {label = 13, sourceNode = AdActionNode {label = 4, name = "G"}},
        SimpleTransition {
          flowOut = M.empty}),
        (NormalPetriNode {label = 14, sourceNode = AdJoinNode {label = 14}},
        SimpleTransition {
          flowOut = M.empty}),
        (NormalPetriNode {label = 15, sourceNode = AdObjectNode {label = 7, name = "F"}},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 21, sourceNode = AdForkNode {label = 13}},1)]}),
        (NormalPetriNode {label = 16, sourceNode = AdInitialNode {label = 17}},
        SimplePlace {
          initial = 1,
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 19},1)]}),
        (NormalPetriNode {label = 17, sourceNode = AdActionNode {label = 1, name = "A"}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 2},1)]}),
        (AuxiliaryPetriNode {label = 18},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 13, sourceNode = AdActionNode {label = 4, name = "G"}},1)]}),
        (AuxiliaryPetriNode {label = 19},
        SimpleTransition {
          flowOut = M.fromList [(NormalPetriNode {label = 15, sourceNode = AdObjectNode {label = 7, name = "F"}},1)]}),
        (NormalPetriNode {label = 20, sourceNode = AdDecisionNode {label = 9}},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 1, sourceNode = AdActionNode {label = 2, name = "B"}},1),
            (NormalPetriNode {label = 17, sourceNode = AdActionNode {label = 1, name = "A"}},1)]}),
        (NormalPetriNode {label = 21, sourceNode = AdForkNode {label = 13}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 5},1),
            (NormalPetriNode {label = 11, sourceNode = AdMergeNode {label = 11}},1),
            (NormalPetriNode {label = 22, sourceNode = AdObjectNode {label = 5, name = "D"}},1)]}),
        (NormalPetriNode {label = 22, sourceNode = AdObjectNode {label = 5, name = "D"}},
        SimplePlace {
          initial = 0,
          flowOut = M.fromList [(NormalPetriNode {label = 14, sourceNode = AdJoinNode {label = 14}},1)]}),
        (AuxiliaryPetriNode {label = 23},
        SimpleTransition {
          flowOut = M.fromList [(NormalPetriNode {label = 11, sourceNode = AdMergeNode {label = 11}},1)]}),
        (NormalPetriNode {label = 24, sourceNode = AdJoinNode {label = 12}},
        SimpleTransition {
          flowOut = M.fromList [(AuxiliaryPetriNode {label = 10},1)]})
      ]
    }
  )),
  (2,(True,PetriLike {
    allNodes = M.fromList [
      (NormalPetriNode {label = 1, sourceNode = AdForkNode {label = 13}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 3, sourceNode = AdObjectNode {label = 5, name = "D"}},1),
          (NormalPetriNode {label = 5, sourceNode = AdObjectNode {label = 6, name = "C"}},1),
          (NormalPetriNode {label = 12, sourceNode = AdMergeNode {label = 11}},1)]}),
      (AuxiliaryPetriNode {label = 2},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 6, sourceNode = AdObjectNode {label = 7, name = "F"}},1)]}),
      (NormalPetriNode {label = 3, sourceNode = AdObjectNode {label = 5, name = "D"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 19, sourceNode = AdJoinNode {label = 14}},1)]}),
      (NormalPetriNode {label = 4, sourceNode = AdActionNode {label = 4, name = "G"}},
      SimpleTransition {
        flowOut = M.empty}),
      (NormalPetriNode {label = 5, sourceNode = AdObjectNode {label = 6, name = "C"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 19, sourceNode = AdJoinNode {label = 14}},1)]}),
      (NormalPetriNode {label = 6, sourceNode = AdObjectNode {label = 7, name = "F"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 1, sourceNode = AdForkNode {label = 13}},1)]}),
      (NormalPetriNode {label = 7, sourceNode = AdDecisionNode {label = 9}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 9, sourceNode = AdActionNode {label = 1, name = "A"}},1),
          (NormalPetriNode {label = 11, sourceNode = AdActionNode {label = 2, name = "B"}},1)]}),
      (NormalPetriNode {label = 8, sourceNode = AdMergeNode {label = 12}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 18},1)]}),
      (NormalPetriNode {label = 9, sourceNode = AdActionNode {label = 1, name = "A"}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 8, sourceNode = AdMergeNode {label = 12}},1)]}),
      (NormalPetriNode {label = 10, sourceNode = AdInitialNode {label = 17}},
      SimplePlace {
        initial = 1,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 2},1)]}),
      (NormalPetriNode {label = 11, sourceNode = AdActionNode {label = 2, name = "B"}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 8, sourceNode = AdMergeNode {label = 12}},1)]}),
      (NormalPetriNode {label = 12, sourceNode = AdMergeNode {label = 11}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 15, sourceNode = AdActionNode {label = 3, name = "E"}},1)]}),
      (AuxiliaryPetriNode {label = 13},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 7, sourceNode = AdDecisionNode {label = 9}},1)]}),
      (NormalPetriNode {label = 14, sourceNode = AdDecisionNode {label = 10}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 13},1), (AuxiliaryPetriNode {label = 17},1)]}),
      (NormalPetriNode {label = 15, sourceNode = AdActionNode {label = 3, name = "E"}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 14, sourceNode = AdDecisionNode {label = 10}},1)]}),
      (NormalPetriNode {label = 16, sourceNode = AdObjectNode {label = 8, name = "H"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 4, sourceNode = AdActionNode {label = 4, name = "G"}},1)]}),
      (AuxiliaryPetriNode {label = 17},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 12, sourceNode = AdMergeNode {label = 11}},1)]}),
      (AuxiliaryPetriNode {label = 18},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 16, sourceNode = AdObjectNode {label = 8, name = "H"}},1)]}),
      (NormalPetriNode {label = 19, sourceNode = AdJoinNode {label = 14}},
      SimpleTransition {
        flowOut = M.empty})
    ]
  })),
  (3,(False,PetriLike {
    allNodes = M.fromList [
      (NormalPetriNode {label = 1, sourceNode = AdJoinNode {label = 12}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 20, sourceNode = AdObjectNode {label = 8, name = "H"}},1)]}),
      (AuxiliaryPetriNode {label = 2},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 6, sourceNode = AdDecisionNode {label = 9}},1)]}),
      (NormalPetriNode {label = 3, sourceNode = AdActionNode {label = 3, name = "E"}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 12, sourceNode = AdDecisionNode {label = 10}},1)]}),
      (AuxiliaryPetriNode {label = 4},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 9, sourceNode = AdObjectNode {label = 7, name = "F"}},1)]}),
      (AuxiliaryPetriNode {label = 5},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 19, sourceNode = AdMergeNode {label = 11}},1)]}),
      (NormalPetriNode {label = 6, sourceNode = AdDecisionNode {label = 9}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 8},1),(AuxiliaryPetriNode {label = 18},1)]}),
      (NormalPetriNode {label = 7, sourceNode = AdObjectNode {label = 5, name = "D"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 11, sourceNode = AdJoinNode {label = 14}},1)]}),
      (AuxiliaryPetriNode {label = 8},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 17, sourceNode = AdObjectNode {label = 1, name = "A"}},1)]}),
      (NormalPetriNode {label = 9, sourceNode = AdObjectNode {label = 7, name = "F"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 16, sourceNode = AdForkNode {label = 13}},1)]}),
      (NormalPetriNode {label = 10, sourceNode = AdActionNode {label = 4, name = "G"}},
      SimpleTransition {
        flowOut = M.empty}),
      (NormalPetriNode {label = 11, sourceNode = AdJoinNode {label = 14}},
      SimpleTransition {
        flowOut = M.empty}),
      (NormalPetriNode {label = 12, sourceNode = AdDecisionNode {label = 10}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 2},1),(AuxiliaryPetriNode {label = 5},1)]}),
      (NormalPetriNode {label = 13, sourceNode = AdObjectNode {label = 6, name = "C"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 11, sourceNode = AdJoinNode {label = 14}},1)]}),
      (NormalPetriNode {label = 14, sourceNode = AdInitialNode {label = 17}},
      SimplePlace {
        initial = 1,
        flowOut = M.fromList [(AuxiliaryPetriNode {label = 4},1)]}),
      (NormalPetriNode {label = 15, sourceNode = AdObjectNode {label = 2, name = "B"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 1, sourceNode = AdJoinNode {label = 12}},1)]}),
      (NormalPetriNode {label = 16, sourceNode = AdForkNode {label = 13}},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 7, sourceNode = AdObjectNode {label = 5, name = "D"}},1),
          (NormalPetriNode {label = 13, sourceNode = AdObjectNode {label = 6, name = "C"}},1),
          (NormalPetriNode {label = 19, sourceNode = AdMergeNode {label = 11}},1)]}),
      (NormalPetriNode {label = 17, sourceNode = AdObjectNode {label = 1, name = "A"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 1, sourceNode = AdJoinNode {label = 12}},1)]}),
      (AuxiliaryPetriNode {label = 18},
      SimpleTransition {
        flowOut = M.fromList [(NormalPetriNode {label = 15, sourceNode = AdObjectNode {label = 2, name = "B"}},1)]}),
      (NormalPetriNode {label = 19, sourceNode = AdMergeNode {label = 11}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 3, sourceNode = AdActionNode {label = 3, name = "E"}},1)]}),
      (NormalPetriNode {label = 20, sourceNode = AdObjectNode {label = 8, name = "H"}},
      SimplePlace {
        initial = 0,
        flowOut = M.fromList [(NormalPetriNode {label = 10, sourceNode = AdActionNode {label = 4, name = "G"}},1)]})
    ]
  }))],
  showSolution = False,
  addText = Nothing
}
