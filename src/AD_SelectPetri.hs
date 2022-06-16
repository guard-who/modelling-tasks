{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_SelectPetri (
  SelectPetriInstance(..),
  SelectPetriConfig(..),
  SelectPetriSolution(..),
  defaultSelectPetriConfig,
  checkSelectPetriConfig,
  selectPetriAlloy,
  selectPetrinet,
  selectPetriTaskDescription
) where

import qualified Data.Map as M ((!), keys)

import AD_Alloy (modulePetrinet)
import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Datatype (UMLActivityDiagram(..), ADNode(..), isInitialNode, isActivityFinalNode, isFlowFinalNode)
import AD_Petrinet (PetriKey(..), convertToPetrinet)

import Modelling.PetriNet.Types (PetriLike(..), Node(flowOut))

import Control.Applicative (Alternative ((<|>)))
import Data.Graph (Graph, graphFromEdges')
import Data.Graph.Automorphism (isIsomorphic)
import Data.List (unfoldr, nubBy)
import Data.String.Interpolate ( i )
import System.Random (mkStdGen, next)       --To be changed from 'next' to 'uniform', not possible as of now due to dependencies
import System.Random.Shuffle (shuffle')

data SelectPetriInstance = SelectPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  numberOfWrongNets :: Int
} deriving (Show, Eq)

data SelectPetriConfig = SelectPetriConfig {
  adConfig :: ADConfig,
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Show)

defaultSelectPetriConfig :: SelectPetriConfig
defaultSelectPetriConfig = SelectPetriConfig {
  adConfig = defaultADConfig,
  supportSTAbsent = Nothing,
  activityFinalsExist = Nothing,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just True
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
  | activityFinalsExist == Just True && activityFinalNodes adConfig == 0
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig <= 0
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

data SelectPetriSolution = SelectPetriSolution {
  matchingNet :: PetriLike PetriKey,
  wrongNets :: [PetriLike PetriKey]
} deriving (Show)

selectPetrinet :: SelectPetriInstance -> SelectPetriSolution
selectPetrinet SelectPetriInstance {
    activityDiagram,
    seed,
    numberOfWrongNets
  } =
  let matchingNet = convertToPetrinet activityDiagram
      seeds = unfoldr (Just . next) (mkStdGen seed)
      wrongNets = take numberOfWrongNets
                  $ nubBy checkIsomorphism
                  $ filter (not . checkIsomorphism matchingNet)
                  $ map (convertToPetrinet . modifyAD activityDiagram) seeds
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

checkIsomorphism :: (Ord a) => PetriLike a -> PetriLike a -> Bool
checkIsomorphism p1 p2 =
  isIsomorphic (petriToGraph p1) (petriToGraph p2)

petriToGraph :: (Ord a) => PetriLike a -> Graph
petriToGraph petri =
  let keys = M.keys $ allNodes petri
      keyToEdgeList k = M.keys $ flowOut $ allNodes petri M.! k
  in fst $ graphFromEdges' $ map (\k -> (k, k, keyToEdgeList k)) keys
