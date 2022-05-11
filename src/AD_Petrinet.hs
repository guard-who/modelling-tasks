{-# LANGUAGE NamedFieldPuns #-}

module AD_Petrinet (
  convertToPetrinet 
) where

import qualified Data.Map as M ((!), adjust, filter, mapMaybeWithKey, foldrWithKey, lookup, insert, delete, empty, singleton, keys)

import AD_Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..)
  )

import Modelling.PetriNet.Types (
  Node(..), 
  PetriLike(..),
  isPlaceNode, isTransitionNode
  )

import Data.Map (Map)
 

convertToPetrinet :: UMLActivityDiagram -> PetriLike String
convertToPetrinet diag = 
  let mt_petri = PetriLike {allNodes = M.empty :: Map String (Node String)}
      st_petri = foldr insertNode mt_petri (nodes diag)
      st_edges_petri = foldr insertEdge st_petri (connections diag)
  in foldr addSupportST st_edges_petri (M.keys $ allNodes st_edges_petri)

addSupportST :: String -> PetriLike String -> PetriLike String
addSupportST sourceKey petri =
  let sourceNode = allNodes petri M.! sourceKey 
      fn = if isPlaceNode sourceNode then isPlaceNode else isTransitionNode
      nodesToBeFixed = M.filter fn $ M.mapMaybeWithKey (\k _ -> M.lookup k (allNodes petri)) $ flowOut sourceNode
  in M.foldrWithKey (addSupportST' sourceKey) petri nodesToBeFixed 

addSupportST' :: String -> String -> Node String -> PetriLike String -> PetriLike String
addSupportST' sourceKey targetKey targetNode petri =
  let supportKey = "H" ++ sourceKey ++ "->" ++ targetKey
      supportNode = if isPlaceNode targetNode then TransitionNode {flowIn = M.singleton sourceKey 1, flowOut = M.singleton targetKey 1}
                    else PlaceNode {initial = 0, flowIn = M.singleton sourceKey 1, flowOut = M.singleton targetKey 1}
      newSourceNode = addFlowOutToNode supportKey $ deleteFlowOutToNode targetKey $ allNodes petri M.! sourceKey
      newTargetNode = addFlowInToNode supportKey $ deleteFlowInToNode sourceKey targetNode
  in PetriLike
      $ M.insert targetKey newTargetNode 
      $ M.insert sourceKey newSourceNode
      $ M.insert supportKey supportNode (allNodes petri)

insertNode :: ADNode -> PetriLike String -> PetriLike String
insertNode node petri =
  case nodeToST node of 
    Just st -> PetriLike $ M.insert (show (label node)) st (allNodes petri)
    Nothing -> petri

nodeToST :: ADNode -> Maybe (Node String)
nodeToST node =
  case node of
    ADInitialNode {} -> Just PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.empty}
    ADActionNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    ADObjectNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    ADDecisionNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    ADMergeNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    ADForkNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    ADJoinNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    _ -> Nothing   

insertEdge :: ADConnection -> PetriLike String -> PetriLike String
insertEdge edge petri =
  let sourceKey = show (from edge)
      targetKey = show (to edge)
      sourceNode = M.lookup sourceKey (allNodes petri)
      targetNode = M.lookup targetKey (allNodes petri)
  in 
  case sourceNode of
    Just _ -> 
      case targetNode of
        Just _ -> PetriLike
                  $ M.adjust (addFlowInToNode sourceKey) targetKey 
                  $ M.adjust (addFlowOutToNode targetKey) sourceKey (allNodes petri)
        Nothing -> petri
    Nothing -> petri

addFlowInToNode :: String -> Node String -> Node String
addFlowInToNode x node = 
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=M.insert x 1 flowIn, flowOut=flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=M.insert x 1 flowIn, flowOut=flowOut}

addFlowOutToNode :: String -> Node String -> Node String
addFlowOutToNode x node = 
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=flowIn, flowOut=M.insert x 1 flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=flowIn, flowOut=M.insert x 1 flowOut}

deleteFlowInToNode :: String -> Node String -> Node String
deleteFlowInToNode x node = 
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=M.delete x flowIn, flowOut=flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=M.delete x flowIn, flowOut=flowOut}

deleteFlowOutToNode :: String -> Node String -> Node String
deleteFlowOutToNode x node = 
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=flowIn, flowOut=M.delete x flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=flowIn, flowOut=M.delete x flowOut}