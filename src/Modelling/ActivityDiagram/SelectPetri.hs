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
  selectPetriTask,
  selectPetriSyntax,
  selectPetriEvaluation,
  selectPetri
  ) where

import qualified Data.Map as M (size, fromList, toList, keys, map, filter)
import qualified Modelling.ActivityDiagram.Petrinet as PK (PetriKey(label))

import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Isomorphism (isPetriIsomorphic)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.PlantUMLConverter (drawADToFile, defaultPlantUMLConvConf)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames, shufflePetri)

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
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import System.Random (next)       --To be changed from 'next' to 'uniform', not possible as of now due to dependencies
import System.Random.Shuffle (shuffle', shuffleM)


data SelectPetriInstance = SelectPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  graphvizCmd :: GraphvizCommand,
  petrinets :: Map Int (Bool, PetriLike PetriKey)
} deriving (Show)

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
    petriLayout,
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
  | any (`notElem` [Dot, Neato, TwoPi, Circo, Fdp]) petriLayout
    = Just "The parameter 'petriLayout' can only contain the options Dot, Neato, TwoPi, Circo and Fdp"
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

checkPetriInstance :: SelectPetriInstance -> SelectPetriConfig -> Maybe String
checkPetriInstance inst SelectPetriConfig {
    numberOfWrongAnswers
  }
  | M.size (M.filter (not . fst) $ petrinets inst) /= numberOfWrongAnswers
    = Just "Number of wrong nets found for given instance is unequal to numberOfWrongAnswers"
  | otherwise
    = Nothing

data SelectPetriSolution = SelectPetriSolution {
  matchingNet :: PetriLike PetriKey,
  wrongNets :: [PetriLike PetriKey]
} deriving (Show)

selectPetrinet :: Int -> Int -> UMLActivityDiagram -> SelectPetriSolution
selectPetrinet numberOfWrongNets seed ad =
  let matchingNet = convertToPetrinet ad
      seeds = unfoldr (Just . next) (mkStdGen seed)
      wrongNets = take numberOfWrongNets
                  $ nubBy isPetriIsomorphic
                  $ filter (not . isPetriIsomorphic matchingNet)
                  $ map (convertToPetrinet . modifyAD ad) seeds
  in SelectPetriSolution {matchingNet=matchingNet, wrongNets=wrongNets}

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
  let mapping = M.map snd $ petrinets task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  petris <- liftIO $
    traverse (\c -> runExceptT
      $ cacheNet path (show . PK.label) c False False True (graphvizCmd task)) mapping
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
  let options = M.keys $ petrinets task
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
      solMap = petrinets task
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
  n <- getRandom
  g' <- getRandom
  layout <- pickRandomLayout config
  let ad = map (snd . shuffleADNames n . failWith id . parseInstance) rinstas
      validInsta =
        head $ filter (isNothing . (`checkPetriInstance` config))
        $ map (\x ->
          SelectPetriInstance {
            activityDiagram=x,
            seed=g',
            graphvizCmd=layout,
            petrinets= selectPetriSolutionToMap g'
              $ shuffleSolutionNets n
              $ selectPetrinet (numberOfWrongAnswers config) n x
          }) ad
  return validInsta

shuffleSolutionNets :: Int -> SelectPetriSolution -> SelectPetriSolution
shuffleSolutionNets n sol = SelectPetriSolution {
  matchingNet = snd $ shufflePetri n (matchingNet sol),
  wrongNets =  map (snd . shufflePetri n) (wrongNets sol)
}

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id