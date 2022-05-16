module AD_MatchComponents (
  matchADComponents,
  matchPetriComponents
) where

import qualified Data.Map as M ((!), insert, delete, keys, empty, null)

import qualified AD_Datatype as AD (
  UMLActivityDiagram(..),
  ADNode(..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)

import AD_Petrinet (PetriKey(..))

import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Data.Map (Map)


matchADComponents :: AD.UMLActivityDiagram -> Map String [Int]
matchADComponents diag =
  let actionLabels = extractLabels AD.isActionNode  
      objectLabels = extractLabels AD.isObjectNode 
      decisionLabels = extractLabels AD.isDecisionNode 
      mergeLabels = extractLabels AD.isMergeNode 
      forkLabels = extractLabels AD.isForkNode
      joinLabels = extractLabels AD.isJoinNode 
      initialLabels = extractLabels AD.isInitialNode 
      activtiyFinalLabels = extractLabels AD.isActivityFinalNode 
      flowFinalLabels = extractLabels AD.isFlowFinalNode 
  in M.insert "ActionNodes" actionLabels $
     M.insert "ObjectNodes" objectLabels $
     M.insert "DecisionNodes" decisionLabels $
     M.insert "MergeNodes" mergeLabels $
     M.insert "ForkNodes" forkLabels $
     M.insert "JoinNodes" joinLabels $
     M.insert "InitialNodes" initialLabels $
     M.insert "ActivityFinalNodes" activtiyFinalLabels $
     M.insert "FlowFinalNodes" flowFinalLabels 
     M.empty
  where extractLabels fn = map AD.label $ filter fn $ AD.nodes diag

--Precondition: petri was generated from diag via convertToPetrinet
matchPetriComponents :: AD.UMLActivityDiagram -> PetriLike PetriKey -> Map String [Int]
matchPetriComponents diag petri =
  let labelMap = M.delete "FlowFinalNodes" $ M.delete "ActivityFinalNodes" $ matchADComponents diag
      supportST = map label $ filter (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri
  in M.insert "SupportST" supportST labelMap

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False