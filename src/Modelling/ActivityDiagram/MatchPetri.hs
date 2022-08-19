{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.MatchPetri (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  pickRandomLayout,
  defaultMatchPetriConfig,
  checkMatchPetriConfig,
  matchPetriComponents,
  matchPetriAlloy,
  matchPetriTaskDescription,
  matchPetriComponentsText,
  extractSupportSTs
) where

import qualified Data.Map as M ((!), keys, null)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram, ADNode(name), isActionNode, isObjectNode, isDecisionNode, isMergeNode, isJoinNode, isInitialNode, isForkNode)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shufflePetri, shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)

import Modelling.Auxiliary.Common (oneOf)
import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Random (MonadRandom)
import Data.GraphViz.Commands (GraphvizCommand(Dot))
import Data.List (sort)
import Data.String.Interpolate ( i )


data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: ADConfig,
  petriLayout :: [GraphvizCommand],
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Show)

pickRandomLayout :: (MonadRandom m) => MatchPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultMatchPetriConfig :: MatchPetriConfig
defaultMatchPetriConfig = MatchPetriConfig
  { adConfig = defaultADConfig,
    petriLayout = [Dot],
    supportSTAbsent = Nothing,
    activityFinalsExist = Nothing,
    avoidAddingSinksForFinals = Nothing,
    noActivityFinalInForkBlocks = Just True
  }

checkMatchPetriConfig :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig conf =
  checkADConfig (adConfig conf)
  <|> checkMatchPetriConfig' conf


checkMatchPetriConfig' :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig' MatchPetriConfig {
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


matchPetriAlloy :: MatchPetriConfig -> String
matchPetriAlloy MatchPetriConfig {
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


mapTypesToLabels :: PetriLike PetriKey -> MatchPetriSolution
mapTypesToLabels petri =
  MatchPetriSolution {
    actionNodes = extractNameLabelTuple isActionNode,
    objectNodes = extractNameLabelTuple isObjectNode,
    decisionNodes = extractLabels isDecisionNode,
    mergeNodes = extractLabels isMergeNode,
    forkNodes = extractLabels isForkNode,
    joinNodes = extractLabels isJoinNode,
    initialNodes = extractLabels isInitialNode,
    supportSTs = sort $ map label $ extractSupportSTs petri
  }
  where
    extractNameLabelTuple fn =
      sort $
      map (\k -> (name $ sourceNode k, label k)) $
      keysByNodeType fn
    extractLabels fn =
      sort $
      map label $
      keysByNodeType fn
    keysByNodeType fn =
      filter (fn . sourceNode)  $
      filter (not . isSupportST) $
      M.keys $ allNodes petri


matchPetriTaskDescription :: String
matchPetriTaskDescription =
  [i|
    Look at the given Activity Diagram and Petrinet, then use the displayed numbers
    at the places and transitions as identifiers for the following tasks:

    a) For every Action in the Activity Diagram, name a tuple with its corresponding node in the petrinet
       (e.g. ("A", 1) if the Action "A" corresponds to 1 in the petrinet)
    b) For every Object Node in the Activity Diagram, name a tuple with its corresponding node in the petrinet
       (e.g. ("O", 2) if the Action "O" corresponds to 2 in the petrinet)
    c) Name all nodes of the petrinet which correspond to Decision Nodes in the Activity Diagram
    d) Name all nodes of the petrinet which correspond to Merge Nodes in the Activity Diagram
    e) Name all nodes of the petrinet which correspond to Fork Nodes in the Activity Diagram
    f) Name all nodes of the petrinet which correspond to Join Nodes in the Activity Diagram
    g) Name all nodes of the petrinet which correspond to Initial Nodes in the Activity Diagram
    h) Name all added support places and support transtions
  |]

matchPetriComponentsText :: MatchPetriInstance -> (UMLActivityDiagram, PetriLike PetriKey, String)
matchPetriComponentsText inst =
  let (ad, petri, solution) = matchPetriComponents inst
      text = [i|
      Solutions for the MatchPetri-Task:

      a) Matchings of Actions to petrinet nodes: #{actionNodes solution}
      b) Matchings of Object Nodes to petrinet nodes: #{objectNodes solution}
      c) Nodes in the petrinet corresponding to Decision Nodes: #{decisionNodes solution}
      d) Nodes in the petrinet corresponding to Merge Nodes: #{mergeNodes solution}
      e) Nodes in the petrinet corresponding to Fork Nodes: #{forkNodes solution}
      f) Nodes in the petrinet corresponding to Join Nodes: #{joinNodes solution}
      g) Nodes in the petrinet corresponding to Initial Nodes: #{initialNodes solution}
      h) Support places and transitions: #{supportSTs solution}
      |]
  in (ad, petri, text)

data MatchPetriSolution = MatchPetriSolution {
  actionNodes :: [(String, Int)],
  objectNodes :: [(String, Int)],
  decisionNodes :: [Int],
  mergeNodes :: [Int],
  forkNodes :: [Int],
  joinNodes :: [Int],
  initialNodes :: [Int],
  supportSTs :: [Int]
} deriving (Show, Eq)

matchPetriComponents :: MatchPetriInstance -> (UMLActivityDiagram, PetriLike PetriKey, MatchPetriSolution)
matchPetriComponents MatchPetriInstance {
  activityDiagram,
  seed
} =
  let ad = snd $ shuffleADNames seed activityDiagram
      petri = snd $ shufflePetri seed $ convertToPetrinet ad
      solution = mapTypesToLabels petri
  in (ad, petri, solution)


extractSupportSTs :: PetriLike PetriKey -> [PetriKey]
extractSupportSTs petri = filter (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False