{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.MatchPetri (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  MatchPetriSolution(..),
  defaultMatchPetriConfig,
  checkMatchPetriConfig,
  mapTypesToLabels,
  matchPetriAlloy,
  matchPetriSolution,
  extractAuxiliaryPetriNodes,
  matchPetriTask,
  matchPetriInitial,
  matchPetriSyntax,
  matchPetriEvaluation,
  matchPetri,
  defaultMatchPetriInstance
) where

import qualified Data.Map as M (empty, fromList, keys)

import qualified Modelling.ActivityDiagram.Config as Config (
  AdConfig (activityFinalNodes, flowFinalNodes),
  )
import Modelling.ActivityDiagram.Auxiliary.PetriValidation (validatePetriConfig)
import qualified Modelling.ActivityDiagram.PetriNet as PK (label)
import qualified Modelling.PetriNet.Types as Petri (Net (nodes))

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Capabilities.PlantUml            (MonadPlantUml)
import Capabilities.WriteFile           (MonadWriteFile)
import Modelling.ActivityDiagram.Alloy  (adConfigToAlloy, modulePetriNet)
import Modelling.ActivityDiagram.Auxiliary.Util (finalNodesAdvice)
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..),
  isActionNode,
  isActivityFinalNode,
  isObjectNode,
  isDecisionNode,
  isFlowFinalNode,
  isMergeNode,
  isJoinNode,
  isInitialNode,
  isForkNode
  )
import Modelling.ActivityDiagram.Isomorphism (petriHasMultipleAutomorphisms)
import Modelling.ActivityDiagram.PetriNet (
  PetriKey (..),
  convertToPetriNet,
  isAuxiliaryPetriNode,
  )
import Modelling.ActivityDiagram.Shuffle (shufflePetri, shuffleAdNames)
import Modelling.ActivityDiagram.Config (
  AdConfig,
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.Auxiliary.Common (getFirstInstance, oneOf)
import Modelling.Auxiliary.Output (
  addPretext,
  extra
  )
import Modelling.PetriNet.Diagram (cacheNet)
import Modelling.PetriNet.Types (
  checkPetriNodeCount,
  DrawSettings (..),
  Net (mapNet),
  PetriLike (..),
  SimpleNode (..),
  SimplePetriLike,
  )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Language,
  Rated,
  OutputCapable,
  ($=<<),
  english,
  german,
  translate,
  translations,
  multipleChoice,
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Bifunctor                   (second)
import Data.Containers.ListUtils (nubOrd)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.List (intersect, sort)
import Data.Map (Map)
import Data.String.Interpolate (i, iii)
import Data.Tuple.Extra                 (dupe)
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)


data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  petriNet :: SimplePetriLike PetriKey,
  plantUMLConf :: PlantUmlConfig,
  petriDrawConf :: DrawSettings,
  showSolution :: Bool,
  addText :: Maybe (Map Language String)
} deriving (Generic, Read, Show)

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: AdConfig,
  -- | generate only activity diagrams with a corresponding Petri net
  -- having a total count of nodes within the given bounds
  countOfPetriNodesBounds :: !(Int, Maybe Int),
  maxInstances :: Maybe Integer,
  hideBranchConditions :: Bool,
  petriLayout :: [GraphvizCommand],
  -- | Whether highlighting on hover should be enabled
  petriSvgHighlighting :: Bool,
  -- | Option to prevent auxiliary PetriNodes from occurring
  auxiliaryPetriNodeAbsent :: Maybe Bool,
  -- | Force presence or absence of new sink transitions for representing finals
  presenceOfSinkTransitionsForFinals :: Maybe Bool,
  -- | Avoid Activity Finals in concurrent flows to reduce confusion
  withActivityFinalInForkBlocks :: !(Maybe Bool),
  printSolution :: Bool,
  extraText :: Maybe (Map Language String)
} deriving (Generic, Read, Show)

pickRandomLayout :: (MonadRandom m) => MatchPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultMatchPetriConfig :: MatchPetriConfig
defaultMatchPetriConfig =
  MatchPetriConfig {
    adConfig = defaultAdConfig {
      Config.activityFinalNodes = 0,
      Config.flowFinalNodes = 2
      },
    countOfPetriNodesBounds = (0, Nothing),
    maxInstances = Just 25,
    hideBranchConditions = False,
    petriLayout = [Dot],
    petriSvgHighlighting = True,
    auxiliaryPetriNodeAbsent = Nothing,
    presenceOfSinkTransitionsForFinals = Nothing,
    withActivityFinalInForkBlocks = Just False,
    printSolution = False,
    extraText = Nothing
  }

checkMatchPetriConfig :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkMatchPetriConfig' conf


checkMatchPetriConfig' :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig' MatchPetriConfig {
    adConfig,
    countOfPetriNodesBounds,
    maxInstances,
    petriLayout,
    auxiliaryPetriNodeAbsent,
    presenceOfSinkTransitionsForFinals,
    withActivityFinalInForkBlocks
  } = validatePetriConfig
        adConfig
        countOfPetriNodesBounds
        maxInstances
        petriLayout
        auxiliaryPetriNodeAbsent
        presenceOfSinkTransitionsForFinals
        withActivityFinalInForkBlocks

matchPetriAlloy :: MatchPetriConfig -> String
matchPetriAlloy MatchPetriConfig {
  adConfig,
  auxiliaryPetriNodeAbsent,
  presenceOfSinkTransitionsForFinals,
  withActivityFinalInForkBlocks
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
            #{f (not <$> withActivityFinalInForkBlocks) "noActivityFinalInForkBlocks"}
          |]
    f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            Nothing -> ""

mapTypesToLabels
  :: Net p n
  => p n PetriKey
  -> MatchPetriSolution
mapTypesToLabels petri =
  MatchPetriSolution {
    actionNodes = extractNameLabelTuple isActionNode,
    activityFinalNodes = extractLabels isActivityFinalNode,
    objectNodes = extractNameLabelTuple isObjectNode,
    decisionNodes = extractLabels isDecisionNode,
    flowFinalNodes = extractLabels isFlowFinalNode,
    mergeNodes = extractLabels isMergeNode,
    forks = extractLabels isForkNode,
    joins = extractLabels isJoinNode,
    initialNodes = extractLabels isInitialNode,
    auxiliaryPetriNodes = sort $ map PK.label $ extractAuxiliaryPetriNodes petri
  }
  where
    extractNameLabelTuple fn =
      sort $
      map (\k -> (name $ sourceNode k, PK.label k)) $
      keysByNodeType fn
    extractLabels fn =
      sort $
      map PK.label $
      keysByNodeType fn
    keysByNodeType fn =
      filter (fn . sourceNode)  $
      filter (not . isAuxiliaryPetriNode) $
      M.keys $ Petri.nodes petri

data MatchPetriSolution = MatchPetriSolution {
  actionNodes :: [(String, Int)],
  objectNodes :: [(String, Int)],
  decisionNodes :: [Int],
  mergeNodes :: [Int],
  forks :: [Int],
  joins :: [Int],
  initialNodes :: [Int],
  activityFinalNodes :: [Int],
  flowFinalNodes :: [Int],
  auxiliaryPetriNodes :: [Int]
} deriving (Generic, Show, Eq, Read)

matchPetriSolution :: MatchPetriInstance -> MatchPetriSolution
matchPetriSolution task = mapTypesToLabels $ petriNet task

petriSolutionPairwiseDisjunct :: MatchPetriSolution -> Bool
petriSolutionPairwiseDisjunct MatchPetriSolution{..} =
  and [ null (xs `intersect` ys) | xs <- allLists, ys <- allLists, xs /= ys ] && length allLists == length (nubOrd allLists)
    where allLists =
            [ map snd actionNodes
            , map snd objectNodes
            , decisionNodes
            , mergeNodes
            , forks
            , joins
            , initialNodes
            , activityFinalNodes
            , flowFinalNodes
            , auxiliaryPetriNodes]

petriSolutionContainsPetriNodes :: MatchPetriSolution -> [PetriKey] -> Bool
petriSolutionContainsPetriNodes MatchPetriSolution{..} = all ((`elem` solutionKeys) . petriKeyToIndex)
  where
    solutionKeys = concat
      [ map snd actionNodes
      , map snd objectNodes
      , decisionNodes
      , mergeNodes
      , forks
      , joins
      , initialNodes
      , activityFinalNodes
      , flowFinalNodes
      , auxiliaryPetriNodes]
    petriKeyToIndex (AuxiliaryPetriNode index) = index
    petriKeyToIndex (FinalPetriNode index _) = index
    petriKeyToIndex (NormalPetriNode index _) = index

extractAuxiliaryPetriNodes :: Net p n => p n PetriKey -> [PetriKey]
extractAuxiliaryPetriNodes petri = filter
  isAuxiliaryPetriNode
  $ M.keys $ Petri.nodes petri

matchPetriTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadPlantUml m,
    MonadThrow m,
    MonadWriteFile m,
    OutputCapable m
    )
  => FilePath
  -> MatchPetriInstance
  -> LangM m
matchPetriTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following Petri net as translation of this activity diagram:"
    german "Betrachten Sie folgendes Petrinetz als Übersetzung dieses Aktivitätsdiagramms:"
  let drawSetting = petriDrawConf task
  image $=<< cacheNet path (mapNet (show . PK.label) $ petriNet task) drawSetting
  paragraph $ translate $ do
    english [iii|
      State each matching of action node and Petri net node,
      each matching of object node and Petri net node,
      the Petri net nodes per other element kind, as well as all auxiliary places
      and auxiliary transitions in the Petri net.
      |]
    german [iii|
      Geben Sie alle Aktionsknoten/Petrinetzknoten-Paare,
      alle Objektknoten/Petrinetzknoten-Paare, die Petrinetzknoten je anderer Elementart
      und alle Hilfsstellen und -transitionen im Petrinetz an.
      |]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example:|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an:|]
    code $ show matchPetriInitial
    translate $ do
      english [iii|
        In this example, the action nodes "A" and "B"
        are matched with the Petri net nodes 1 and 2,
        the Petri net nodes 5 and 7 correspond to decision nodes,
        the Petri net nodes 13, 14 and 15
        are auxiliary places or auxiliary transitions,
        the Petri net node 16 corresponds to an activity final node,
        and no Petri net node corresponds to a flow final node.
        |]
      german [iii|
        In diesem Beispiel sind etwa die Aktionsknoten "A" und "B"
        den Petrinetzknoten 1 und 2 zugeordnet,
        die Petrinetzknoten 5 und 7 entsprechen Verzweigungsknoten,
        die Petrinetzknoten 13, 14 und 15 sind Hilfsstellen oder -transitionen,
        der Petrinetzknoten 16 entspricht einem Aktivitätsende
        und kein Petrinetzknoten entspricht einem Flussende.
        |]
    pure ()
  finalNodesAdvice False

  extra $ addText task

  pure ()

matchPetriInitial :: MatchPetriSolution
matchPetriInitial = MatchPetriSolution {
  actionNodes = [("A", 1), ("B", 2)],
  activityFinalNodes = [16],
  objectNodes = [("C", 3), ("D", 4)],
  decisionNodes = [5, 7],
  flowFinalNodes = [],
  mergeNodes = [6, 8],
  forks = [10],
  joins = [11],
  initialNodes = [12],
  auxiliaryPetriNodes = [13, 14, 15]
}

matchPetriSyntax
  :: OutputCapable m
  => MatchPetriInstance
  -> MatchPetriSolution
  -> LangM m
matchPetriSyntax task sub = addPretext $ do
  let adNames = map name $ filter (\n -> isActionNode n || isObjectNode n) $ nodes $ activityDiagram task
      subNames = map fst (actionNodes sub) ++ map fst (objectNodes sub)
      petriNodeKeys = M.keys $ allNodes $ petriNet task
      petriLabels = map PK.label petriNodeKeys
      subLabels =
        map snd (actionNodes sub)
        ++ map snd (objectNodes sub)
        ++ decisionNodes sub
        ++ mergeNodes sub
        ++ forks sub
        ++ joins sub
        ++ initialNodes sub
        ++ auxiliaryPetriNodes sub
  assertion (all (`elem` adNames) subNames) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"
  assertion (all (`elem` petriLabels) subLabels) $ translate $ do
    english "Referenced Petri net nodes were provided within task?"
    german "Referenzierte Petrinetzknoten sind Bestandteil der Aufgabenstellung?"
  assertion (petriSolutionContainsPetriNodes sub petriNodeKeys) $ translate $ do
    english "All petri net nodes are associated to an element in the activity diagram?"
    german "Alle Petrinetzknoten sind einem Element in dem Aktivitätsdiagramm zugeordned?"
  assertion (petriSolutionPairwiseDisjunct sub) $ translate $ do
    english "All petri net nodes are associated uniquely?"
    german "Alle Petrinetzknoten sind eindeutig zugeordnet?"
  assertion (all (`elem` subNames) adNames) $ translate $ do
    english "All action and object nodes are referenced?"
    german "Alle Aktions- und Objektknoten wurden referenziert?"
  assertion (length subNames == length (nubOrd subNames)) $ translate $ do
    english "All action and object nodes were referenced exactly once?"
    german "Alle Aktions- und Objektknoten wurden genau einmal referenziert?"
  pure ()

matchPetriEvaluation
  :: OutputCapable m
  => MatchPetriInstance
  -> MatchPetriSolution
  -> Rated m
matchPetriEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "answer parts"
        german "Teilantworten"
      sol = matchPetriSolution task
      maybeSolutionString =
        if showSolution task
        then Just $ show sol
        else Nothing
      solution = matchPetriSolutionMap sol
      sub' = M.keys $ matchPetriSolutionMap sub
  multipleChoice DefiniteArticle as maybeSolutionString solution sub'

matchPetriSolutionMap
  :: MatchPetriSolution
  -> Map (Int, Either [(String, Int)] [Int]) Bool
matchPetriSolutionMap MatchPetriSolution {..} =
  let xs = [
        Left $ sort actionNodes,
        Left $ sort objectNodes,
        Right $ sort decisionNodes,
        Right $ sort mergeNodes,
        Right $ sort forks,
        Right $ sort joins,
        Right $ sort initialNodes,
        Right $ sort activityFinalNodes,
        Right $ sort flowFinalNodes,
        Right $ sort auxiliaryPetriNodes
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

matchPetri
  :: (MonadAlloy m, MonadThrow m)
  => MatchPetriConfig
  -> Int
  -> Int
  -> m MatchPetriInstance
matchPetri config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchPetriTask config) g

getMatchPetriTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => MatchPetriConfig
  -> RandT g m MatchPetriInstance
getMatchPetriTask config = do
  alloyInstances <- getInstances
    (maxInstances config)
    Nothing
    $ matchPetriAlloy config
  randomInstances <- shuffleM alloyInstances >>= mapM parseInstance
  activityDiagrams <- mapM (fmap snd . shuffleAdNames) randomInstances
  (ad, petri) <- getFirstInstance
        $ filter (not . petriHasMultipleAutomorphisms . snd)
        $ filter (checkPetriNodeCount (countOfPetriNodesBounds config) . snd)
        $ map (second convertToPetriNet . dupe) activityDiagrams
  shuffledPetri <- snd <$> shufflePetri petri
  layout <- pickRandomLayout config
  return $ MatchPetriInstance {
    activityDiagram=ad,
    petriNet = shuffledPetri,
    plantUMLConf =
      PlantUmlConfig {
        suppressNodeNames = False,
        suppressBranchConditions = hideBranchConditions config
      },
    petriDrawConf =
      DrawSettings {
        withPlaceNames = True,
        withSvgHighlighting = petriSvgHighlighting config,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = layout
      },
    showSolution = printSolution config,
    addText = extraText config
  }

defaultMatchPetriInstance :: MatchPetriInstance
defaultMatchPetriInstance = MatchPetriInstance
  { activityDiagram = UMLActivityDiagram
    { nodes =
      [ AdActionNode
        { label = 1 , name = "C" }
      , AdActionNode
        { label = 2
        , name = "H" }
      , AdActionNode
        { label = 3
        , name = "A" }
      , AdActionNode
        { label = 4
        , name = "E" }
      , AdObjectNode
        { label = 5
        , name = "F" }
      , AdObjectNode
        { label = 6
        , name = "D" }
      , AdObjectNode
        { label = 7
        , name = "G" }
      , AdObjectNode
        { label = 8
        , name = "B" }
      , AdDecisionNode
        { label = 9 }
      , AdDecisionNode
        { label = 10 }
      , AdMergeNode
        { label = 11 }
      , AdMergeNode
        { label = 12 }
      , AdForkNode
        { label = 13 }
      , AdJoinNode
        { label = 14 }
      , AdActivityFinalNode
        { label = 15 }
      , AdFlowFinalNode
        { label = 16 }
      , AdInitialNode
        { label = 17 } ]
    , connections =
      [ AdConnection
        { from = 1
        , to = 12
        , guard = "" }
      , AdConnection
        { from = 2
        , to = 6
        , guard = "" }
      , AdConnection
        { from = 3
        , to = 4
        , guard = "" }
      , AdConnection
        { from = 4
        , to = 13
        , guard = "" }
      , AdConnection
        { from = 5
        , to = 12
        , guard = "" }
      , AdConnection
        { from = 6
        , to = 14
        , guard = "" }
      , AdConnection
        { from = 7
        , to = 15
        , guard = "" }
      , AdConnection
        { from = 8
        , to = 7
        , guard = "" }
      , AdConnection
        { from = 9
        , to = 11
        , guard = "a" }
      , AdConnection
        { from = 9
        , to = 14
        , guard = "b" }
      , AdConnection
        { from = 10
        , to = 1
        , guard = "b" }
      , AdConnection
        { from = 10
        , to = 5
        , guard = "c" }
      , AdConnection
        { from = 11
        , to = 10
        , guard = "" }
      , AdConnection
        { from = 12
        , to = 9
        , guard = "" }
      , AdConnection
        { from = 13
        , to = 2
        , guard = "" }
      , AdConnection
        { from = 13
        , to = 8
        , guard = "" }
      , AdConnection
        { from = 13
        , to = 11
        , guard = "" }
      , AdConnection
        { from = 14
        , to = 16
        , guard = "" }
      , AdConnection
        { from = 17
        , to = 3
        , guard = "" } ] }
  , petriNet = PetriLike
    { allNodes = M.fromList
      [
        ( NormalPetriNode
          { label = 1
          , sourceNode = AdDecisionNode
            { label = 10 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 7
                , sourceNode = AdActionNode
                  { label = 1
                  , name = "C" } }
              , 1 )
            ,
              ( AuxiliaryPetriNode
                { label = 14 }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 2
          , sourceNode = AdJoinNode
            { label = 14 } }
        , SimpleTransition
          { flowOut = M.empty } )
      ,
        ( NormalPetriNode
          { label = 3
          , sourceNode = AdInitialNode
            { label = 17 } }
        , SimplePlace
          { initial = 1
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 19
                , sourceNode = AdActionNode
                  { label = 3
                  , name = "A" } }
              , 1 ) ] } )
      ,
        ( FinalPetriNode
          { label = 4, sourceNode = AdActivityFinalNode {label = 15} }
        , SimpleTransition
          { flowOut = M.empty } )
      ,
        ( NormalPetriNode
          { label = 5
          , sourceNode = AdMergeNode
            { label = 11 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 21 }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 6
          , sourceNode = AdObjectNode
            { label = 6
            , name = "D" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 2
                , sourceNode = AdJoinNode
                  { label = 14 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 7
          , sourceNode = AdActionNode
            { label = 1
            , name = "C" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 13
                , sourceNode = AdMergeNode
                  { label = 12 } }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 8 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 13
                , sourceNode = AdMergeNode
                  { label = 12 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 9
          , sourceNode = AdObjectNode
            { label = 7
            , name = "G" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( FinalPetriNode
                { label = 4, sourceNode = AdActivityFinalNode {label = 15} }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 10
          , sourceNode = AdActionNode
            { label = 2
            , name = "H" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 6
                , sourceNode = AdObjectNode
                  { label = 6
                  , name = "D" } }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 11 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 12
                , sourceNode = AdForkNode
                  { label = 13 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 12
          , sourceNode = AdForkNode
            { label = 13 } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 5
                , sourceNode = AdMergeNode
                  { label = 11 } }
              , 1 )
            ,
              ( AuxiliaryPetriNode
                { label = 15 }
              , 1 )
            ,
              ( NormalPetriNode
                { label = 22
                , sourceNode = AdObjectNode
                  { label = 8
                  , name = "B" } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 13
          , sourceNode = AdMergeNode
            { label = 12 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 23 }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 14 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 25
                , sourceNode = AdObjectNode
                  { label = 5
                  , name = "F" } }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 15 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 10
                , sourceNode = AdActionNode
                  { label = 2
                  , name = "H" } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 16
          , sourceNode = AdActionNode
            { label = 4
            , name = "E" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 11 }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 17 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 9
                , sourceNode = AdObjectNode
                  { label = 7
                  , name = "G" } }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 18 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 5
                , sourceNode = AdMergeNode
                  { label = 11 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 19
          , sourceNode = AdActionNode
            { label = 3
            , name = "A" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 20 }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 20 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 16
                , sourceNode = AdActionNode
                  { label = 4
                  , name = "E" } }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 21 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 1
                , sourceNode = AdDecisionNode
                  { label = 10 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 22
          , sourceNode = AdObjectNode
            { label = 8
            , name = "B" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 17 }
              , 1 ) ] } )
      ,
        ( AuxiliaryPetriNode
          { label = 23 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 24
                , sourceNode = AdDecisionNode
                  { label = 9 } }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 24
          , sourceNode = AdDecisionNode
            { label = 9 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalPetriNode
                { label = 2
                , sourceNode = AdJoinNode
                  { label = 14 } }
              , 1 )
            ,
              ( AuxiliaryPetriNode
                { label = 18 }
              , 1 ) ] } )
      ,
        ( NormalPetriNode
          { label = 25
          , sourceNode = AdObjectNode
            { label = 5
            , name = "F" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( AuxiliaryPetriNode
                { label = 8 }
              , 1 ) ] } ) ] }
  , plantUMLConf = defaultPlantUmlConfig
  , petriDrawConf =
    DrawSettings {
      withPlaceNames = True,
      withSvgHighlighting = True,
      withTransitionNames = True,
      with1Weights = False,
      withGraphvizCommand = Dot
    },
  showSolution = False,
  addText = Nothing
  }
