{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.Petrinet (
  PetriKey (..),
  convertToPetrinet
) where

import qualified Data.Map as M ((!), adjust, filter, fromList, mapMaybeWithKey, foldrWithKey, lookup, insert, delete, empty, singleton, keys)

import qualified Modelling.ActivityDiagram.Datatype as AD (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..)
  )

import Modelling.PetriNet.Types (
  Net (..),
  Node(..),
  PetriLike(..),
  PetriNode (..),
  )

import Data.Map (Map)
import Data.List (find)
import Modelling.ActivityDiagram.Datatype (
  isActivityFinalNode, isFlowFinalNode
  )


data PetriKey = SupportST {label :: Int} | NormalST {label :: Int, sourceNode :: AD.ADNode} deriving (Eq, Show)

instance Ord PetriKey where
  pk1 `compare` pk2 = label pk1 `compare` label pk2

convertToPetrinet :: AD.UMLActivityDiagram -> PetriLike Node PetriKey
convertToPetrinet diag =
  let mt_petri = PetriLike {allNodes = M.empty :: Map PetriKey (Node PetriKey)}
      st_petri = foldr insertNode mt_petri (AD.nodes diag)
      st_edges_petri = foldr insertEdge st_petri (AD.connections diag)
      st_support_petri = foldr addSupportST st_edges_petri (M.keys $ allNodes st_edges_petri)
  in relabelPetri $ removeFinalPlaces st_support_petri

-- Relabels petrinet nodes in order to avoid "missing" numbers resulting from the creation of sink transitions
relabelPetri
  :: Net p n
  => p n PetriKey
  -> p n PetriKey
relabelPetri petri =
  let labels = map label $ M.keys $ nodes petri
      relabeling = M.fromList $ zip labels [1..(length labels)]
  in mapNet (updatePetriKey relabeling) petri

updatePetriKey :: Map Int Int -> PetriKey -> PetriKey
updatePetriKey relabeling key =
  case key of
    NormalST {label, sourceNode} -> NormalST {label=relabel label, sourceNode=sourceNode}
    SupportST {label} -> SupportST {label=relabel label}
  where relabel n = relabeling M.! n

removeFinalPlaces
  :: Net p n
  => p n PetriKey
  -> p n PetriKey
removeFinalPlaces petri = foldr removeIfFinal petri (M.keys $ nodes petri)

removeIfFinal
  :: Net p n
  => PetriKey
  -> p n PetriKey
  -> p n PetriKey
removeIfFinal key petri =
  case key of
    NormalST {sourceNode} ->
      if isActivityFinalNode sourceNode || isFlowFinalNode sourceNode then
         deleteNode key petri
      else petri
    _ -> petri

addSupportST :: PetriKey -> PetriLike Node PetriKey -> PetriLike Node PetriKey
addSupportST sourceKey petri =
  let sourceNode = allNodes petri M.! sourceKey
      fn = if isPlaceNode sourceNode then isPlaceNode else isTransitionNode
      nodesToBeFixed = M.filter fn $ M.mapMaybeWithKey (\k _ -> M.lookup k (allNodes petri)) $ flowOut sourceNode
  in M.foldrWithKey (addSupportST' sourceKey) petri nodesToBeFixed

addSupportST'
  :: PetriKey
  -> PetriKey
  -> Node PetriKey
  -> PetriLike Node PetriKey
  -> PetriLike Node PetriKey
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

insertNode
  :: Net p n
  => AD.ADNode
  -> p n PetriKey
  -> p n PetriKey
insertNode =
  uncurry repsertNode . nodeToST

nodeToST :: AD.ADNode -> (PetriKey, Maybe Int)
nodeToST node =
  case node of
    AD.ADInitialNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 1
      )
    AD.ADActionNode {label} -> (NormalST{label=label, sourceNode=node},
      Nothing
      )
    AD.ADObjectNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 0
      )
    AD.ADDecisionNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 0
      )
    AD.ADMergeNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 0
      )
    AD.ADForkNode {label} -> (NormalST{label=label, sourceNode=node},
      Nothing
      )
    AD.ADJoinNode {label} -> (NormalST{label=label, sourceNode=node},
      Nothing
      )
    AD.ADActivityFinalNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 0
      )
    AD.ADFlowFinalNode {label} -> (NormalST{label=label, sourceNode=node},
      Just 0
      )

insertEdge
  :: AD.ADConnection
  -> PetriLike Node PetriKey
  -> PetriLike Node PetriKey
insertEdge edge petri =
  let sourceKey = find (\k -> label k == AD.from edge) $ M.keys $ allNodes petri
      targetKey = find (\k -> label k == AD.to edge) $ M.keys $ allNodes petri
  in
  case sourceKey of
    Just sk ->
      case targetKey of
        Just tk -> PetriLike
                  $ M.adjust (addFlowInToNode sk) tk
                  $ M.adjust (addFlowOutToNode tk) sk (allNodes petri)
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
