{-# LANGUAGE NamedFieldPuns #-}

module AD_Petrinet (
  convertToPetrinet 
) where

import qualified Data.Map as M (adjust, lookup, insert, empty)

import AD_Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..)
  )

import PetriStub (
  Node(..), 
  PetriLike(..)
  )

import Data.Map (Map)
 

convertToPetrinet :: UMLActivityDiagram -> PetriLike String
convertToPetrinet diag = 
  let mt_petri = PetriLike {allNodes = M.empty :: Map String (Node String)}
      st_petri = foldr insertNode mt_petri (nodes diag)
  in foldr insertEdge st_petri (connections diag)




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
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=(M.insert x 1 flowIn), flowOut=flowOut}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=(M.insert x 1 flowIn), flowOut=flowOut}

addFlowOutToNode :: String -> Node String -> Node String
addFlowOutToNode x node = 
  case node of
    PlaceNode {initial, flowIn, flowOut} -> PlaceNode {initial=initial, flowIn=flowIn, flowOut=(M.insert x 1 flowOut)}
    TransitionNode {flowIn, flowOut} -> TransitionNode {flowIn=flowIn, flowOut=(M.insert x 1 flowOut)}

