{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.MatchPetri (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  defaultMatchPetriConfig,
  checkMatchPetriConfig,
  matchPetriComponents,
  matchPetriAlloy,
  matchPetriTaskDesciption,
  matchPetriComponentsText,
  extractSupportSTs
) where

import qualified Data.Map as M ((!), insert, keys, empty, null, map)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram, isActionNode, isObjectNode, isDecisionNode, isMergeNode, isJoinNode, isInitialNode, isForkNode)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shufflePetri, shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)

import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Control.Applicative (Alternative ((<|>)))
import Data.List (sort)
import Data.Map (Map)
import Data.String.Interpolate ( i )


data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: ADConfig,
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Show)


defaultMatchPetriConfig :: MatchPetriConfig
defaultMatchPetriConfig = MatchPetriConfig
  { adConfig = defaultADConfig,
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


mapTypesToLabels :: PetriLike PetriKey -> Map String [Int]
mapTypesToLabels petri =
  let actionLabels = extractLabels isActionNode
      objectLabels = extractLabels isObjectNode
      decisionLabels = extractLabels isDecisionNode
      mergeLabels = extractLabels isMergeNode
      forkLabels = extractLabels isForkNode
      joinLabels = extractLabels isJoinNode
      initialLabels = extractLabels isInitialNode
  in M.insert "ActionNodes" actionLabels $
     M.insert "ObjectNodes" objectLabels $
     M.insert "DecisionNodes" decisionLabels $
     M.insert "MergeNodes" mergeLabels $
     M.insert "ForkNodes" forkLabels $
     M.insert "JoinNodes" joinLabels $
     M.insert "InitialNodes" initialLabels
     M.empty
  where
    extractLabels fn =
      map label $
      filter (fn . sourceNode)  $
      filter (not . isSupportST) $
      M.keys $ allNodes petri


matchPetriTaskDesciption :: String
matchPetriTaskDesciption =
  [i|
    Look at the given Activity Diagram and Petrinet, then use the displayed numbers
    at the places and transitions as identifiers for the following tasks:

    a) Name all nodes of the petrinet which correspond to Actions in the Activity Diagram
    b) Name all nodes of the petrinet which correspond to Object Nodes in the Activity Diagram
    c) Name all nodes of the petrinet which correspond to Decision Nodes in the Activity Diagram
    d) Name all nodes of the petrinet which correspond to Merge Nodes in the Activity Diagram
    e) Name all nodes of the petrinet which correspond to Fork Nodes in the Activity Diagram
    f) Name all nodes of the petrinet which correspond to Join Nodes in the Activity Diagram
    g) Name all nodes of the petrinet which correspond to Initial Nodes in the Activity Diagram
    h) Name all added support places and support transtions
  |]

matchPetriComponentsText :: MatchPetriInstance -> (UMLActivityDiagram, PetriLike PetriKey, String)
matchPetriComponentsText inst =
  let (ad, petri, solutions) = matchPetriComponents inst
      text = [i|
      Solutions for the MatchPetri-Task:

      a) Nodes in the petrinet corresponding to Actions: #{solutions M.! "ActionNodes"}
      b) Nodes in the petrinet corresponding to Object Nodes: #{solutions M.! "ObjectNodes"}
      c) Nodes in the petrinet corresponding to Decision Nodes: #{solutions M.! "DecisionNodes"}
      d) Nodes in the petrinet corresponding to Merge Nodes: #{solutions M.! "MergeNodes"}
      e) Nodes in the petrinet corresponding to Fork Nodes: #{solutions M.! "ForkNodes"}
      f) Nodes in the petrinet corresponding to Join Nodes: #{solutions M.! "JoinNodes"}
      g) Nodes in the petrinet corresponding to Initial Nodes: #{solutions M.! "InitialNodes"}
      h) Support places and transitions: #{solutions M.! "SupportST"}
      |]
  in (ad, petri, text)

matchPetriComponents :: MatchPetriInstance -> (UMLActivityDiagram, PetriLike PetriKey, Map String [Int])
matchPetriComponents MatchPetriInstance {
  activityDiagram,
  seed
} =
  let (relabeling, petri) = shufflePetri seed $ convertToPetrinet activityDiagram
      labelMap = M.map (sort . map (relabeling M.!)) $ mapTypesToLabels petri
      supportST = sort $ map label $ extractSupportSTs petri
      ad = snd $ shuffleADNames seed activityDiagram
  in (ad, petri, M.insert "SupportST" supportST labelMap)


extractSupportSTs :: PetriLike PetriKey -> [PetriKey]
extractSupportSTs petri = filter (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False