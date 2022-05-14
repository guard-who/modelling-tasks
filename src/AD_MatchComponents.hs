module AD_MatchComponents (
  matchADComponents
) where

import qualified Data.Map as M (insert, empty)

import AD_Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)

import Data.Map (Map)


matchADComponents :: UMLActivityDiagram -> Map String [Int]
matchADComponents diag =
  let actionLabels = extractLabels isActionNode  
      objectLabels = extractLabels isObjectNode 
      decisionLabels = extractLabels isDecisionNode 
      mergeLabels = extractLabels isMergeNode 
      forkLabels = extractLabels isForkNode
      joinLabels = extractLabels isJoinNode 
      initialLabels = extractLabels isInitialNode 
      activtiyFinalLabels = extractLabels isActivityFinalNode 
      flowFinalLabels = extractLabels isFlowFinalNode 
  in M.insert "ActionNodes" actionLabels $
     M.insert "ObjectNodes" objectLabels $
     M.insert "DecisionNodes" decisionLabels $
     M.insert "MergeNodes" mergeLabels $
     M.insert "ForkNodes" forkLabels $
     M.insert "JoinNodes" joinLabels $
     M.insert "InitialNodes" initialLabels $
     M.insert "ActivityFinalNodes" activtiyFinalLabels $
     M.insert "FlowFinalNodes" flowFinalLabels $
     M.empty
  where extractLabels fn = map label $ filter fn $ nodes diag