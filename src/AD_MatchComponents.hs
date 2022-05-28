{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module AD_MatchComponents (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  defaultMatchPetriConfig,
  checkMatchPetriConfig,
  matchPetriComponents,
  matchPetriAlloy
) where

import qualified Data.Map as M ((!), insert, keys, empty, null, map)
import qualified AD_Datatype as AD (ADNode(label))

import AD_Datatype (
  UMLActivityDiagram(..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode)

import AD_Petrinet (PetriKey(..), convertToPetrinet)
import AD_Shuffle (shufflePetri)
import AD_Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import AD_Alloy (modulePetrinet)

import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Control.Applicative (Alternative ((<|>)))
import Data.Map (Map)
import Data.String.Interpolate ( i )



data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: ADConfig,
  supportSTExist :: Maybe Bool,           -- Option to force support STs to occur
  activityFinalsExist :: Maybe Bool,      -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool -- Avoid having to add new sink transitions for representing finals
} deriving (Show)


defaultMatchPetriConfig :: MatchPetriConfig
defaultMatchPetriConfig = MatchPetriConfig
  { adConfig = defaultADConfig,
    supportSTExist = Nothing,
    activityFinalsExist = Nothing,
    avoidAddingSinksForFinals = Nothing
  }

checkMatchPetriConfig :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig conf =
  checkADConfig (adConfig conf)
  <|> checkMatchPetriConfig' conf


checkMatchPetriConfig' :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig' MatchPetriConfig {
    adConfig,
    activityFinalsExist,
    avoidAddingSinksForFinals
  }
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'allowActivityFinals' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig <= 0
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing


matchPetriAlloy :: MatchPetriConfig -> String
matchPetriAlloy MatchPetriConfig {
  adConfig,
  supportSTExist,
  activityFinalsExist,
  avoidAddingSinksForFinals
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            #{f supportSTExist "supportSTExist"}
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""


mapTypesToLabels :: UMLActivityDiagram -> Map String [Int]
mapTypesToLabels diag =
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
  where extractLabels fn = map AD.label $ filter fn $ nodes diag

matchPetriComponents :: MatchPetriInstance -> (PetriLike PetriKey, Map String [Int])
matchPetriComponents MatchPetriInstance {
  activityDiagram,
  seed
} =
  let (relabeling, petri) = shufflePetri seed $ convertToPetrinet activityDiagram
      labelMap = M.map (map (relabeling M.!)) $ mapTypesToLabels activityDiagram
      supportST = map label $ filter (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri
  in (petri, M.insert "SupportST" supportST labelMap)

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False