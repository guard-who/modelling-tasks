{-# LANGUAGE NamedFieldPuns #-}

module AD_Petrinet (
  PetriKey (..),
  convertToPetrinet
) where

import qualified Data.Map as M ((!), adjust, filter, mapMaybeWithKey, foldrWithKey, lookup, insert, delete, empty, singleton, keys)

import qualified AD_Datatype as AD (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..),
  getFinalNodes
  )

import Modelling.PetriNet.Types (
  Node(..),
  PetriLike(..),
  isPlaceNode, isTransitionNode
  )

import Data.Map (Map)


data PetriKey = SupportST {label :: Int} | NormalST {label :: Int} deriving (Ord, Eq, Show)

convertToPetrinet :: AD.UMLActivityDiagram -> PetriLike PetriKey
convertToPetrinet diag =
  let mt_petri = PetriLike {allNodes = M.empty :: Map PetriKey (Node PetriKey)}
      st_petri = foldr insertNode mt_petri (AD.nodes diag)
      st_edges_petri = foldr insertEdge st_petri (AD.connections diag)
      st_support_petri = foldr addSupportST st_edges_petri (M.keys $ allNodes st_edges_petri)
  in removeFinalPlaces diag st_support_petri

removeFinalPlaces :: AD.UMLActivityDiagram -> PetriLike PetriKey -> PetriLike PetriKey
removeFinalPlaces diag petri = foldr (removeIfFinal diag) petri (M.keys $ allNodes petri)

removeIfFinal :: AD.UMLActivityDiagram -> PetriKey -> PetriLike PetriKey -> PetriLike PetriKey
removeIfFinal diag key petri =
  let flowInKeys = M.keys $ flowIn $ allNodes petri M.! key
  in
  case key of
    NormalST {label} -> if isFinalNode label then
                          PetriLike $ M.delete key $ allNodes $ foldr (removeEdgeToFinal key) petri flowInKeys
                        else petri
    _ -> petri
  where isFinalNode n = elem n $ map AD.label $ AD.getFinalNodes diag

removeEdgeToFinal :: PetriKey -> PetriKey -> PetriLike PetriKey -> PetriLike PetriKey
removeEdgeToFinal x key petri =
  let updatedNode = deleteFlowOutToNode x $ allNodes petri M.! key
  in PetriLike $ M.insert key updatedNode (allNodes petri)

addSupportST :: PetriKey -> PetriLike PetriKey -> PetriLike PetriKey
addSupportST sourceKey petri =
  let sourceNode = allNodes petri M.! sourceKey
      fn = if isPlaceNode sourceNode then isPlaceNode else isTransitionNode
      nodesToBeFixed = M.filter fn $ M.mapMaybeWithKey (\k _ -> M.lookup k (allNodes petri)) $ flowOut sourceNode
  in M.foldrWithKey (addSupportST' sourceKey) petri nodesToBeFixed

addSupportST' :: PetriKey -> PetriKey -> Node PetriKey -> PetriLike PetriKey -> PetriLike PetriKey
addSupportST' sourceKey targetKey targetNode petri =
  let supportKey = SupportST {label = (+ 1) $ maximum $ map label $ M.keys $ allNodes petri}
      supportNode = if isPlaceNode targetNode then TransitionNode {flowIn = M.singleton sourceKey 1, flowOut = M.singleton targetKey 1}
                    else PlaceNode {initial = 0, flowIn = M.singleton sourceKey 1, flowOut = M.singleton targetKey 1}
      newSourceNode = addFlowOutToNode supportKey $ deleteFlowOutToNode targetKey $ allNodes petri M.! sourceKey
      newTargetNode = addFlowInToNode supportKey $ deleteFlowInToNode sourceKey targetNode
  in PetriLike
      $ M.insert targetKey newTargetNode
      $ M.insert sourceKey newSourceNode
      $ M.insert supportKey supportNode (allNodes petri)

insertNode :: AD.ADNode -> PetriLike PetriKey -> PetriLike PetriKey
insertNode node petri =
  case nodeToST node of
    Just st -> PetriLike $ M.insert (NormalST{label = AD.label node}) st (allNodes petri)
    Nothing -> petri

nodeToST :: AD.ADNode -> Maybe (Node PetriKey)
nodeToST node =
  case node of
    AD.ADInitialNode {} -> Just PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.empty}
    AD.ADActionNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    AD.ADObjectNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    AD.ADDecisionNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    AD.ADMergeNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    AD.ADForkNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    AD.ADJoinNode {} -> Just TransitionNode {flowIn = M.empty, flowOut = M.empty}
    AD.ADActivityFinalNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}
    AD.ADFlowFinalNode {} -> Just PlaceNode {initial = 0, flowIn = M.empty, flowOut = M.empty}

insertEdge :: AD.ADConnection -> PetriLike PetriKey -> PetriLike PetriKey
insertEdge edge petri =
  let sourceKey = NormalST{label = AD.from edge}
      targetKey = NormalST{label = AD.to edge}
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

addFlowInToNode :: PetriKey -> Node PetriKey -> Node PetriKey
addFlowInToNode x node =
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=M.insert x 1 flowIn, flowOut=flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=M.insert x 1 flowIn, flowOut=flowOut}

addFlowOutToNode :: PetriKey -> Node PetriKey -> Node PetriKey
addFlowOutToNode x node =
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=flowIn, flowOut=M.insert x 1 flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=flowIn, flowOut=M.insert x 1 flowOut}

deleteFlowInToNode :: PetriKey -> Node PetriKey -> Node PetriKey
deleteFlowInToNode x node =
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=M.delete x flowIn, flowOut=flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=M.delete x flowIn, flowOut=flowOut}

deleteFlowOutToNode :: PetriKey -> Node PetriKey -> Node PetriKey
deleteFlowOutToNode x node =
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=flowIn, flowOut=M.delete x flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=flowIn, flowOut=M.delete x flowOut}