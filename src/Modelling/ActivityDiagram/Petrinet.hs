{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.Petrinet (
  PetriKey (..),
  convertToPetrinet,
  convertToSimple,
) where

import qualified Data.Map as M (
  (!),
  filter,
  foldrWithKey,
  fromList,
  keys,
  lookup,
  mapMaybeWithKey,
  )

import qualified Modelling.ActivityDiagram.Datatype as AD (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..)
  )

import Modelling.PetriNet.Types (
  Net (..),
  PetriNode (..),
  SimplePetriLike,
  )

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Modelling.ActivityDiagram.Datatype (
  isActivityFinalNode, isFlowFinalNode
  )


data PetriKey = SupportST {label :: Int} | NormalST {label :: Int, sourceNode :: AD.ADNode} deriving (Eq, Show)

instance Ord PetriKey where
  pk1 `compare` pk2 = label pk1 `compare` label pk2

convertToSimple :: AD.UMLActivityDiagram -> SimplePetriLike PetriKey
convertToSimple = convertToPetrinet

convertToPetrinet :: Net p n => AD.UMLActivityDiagram -> p n PetriKey
convertToPetrinet diag =
  let st_petri = foldr insertNode emptyNet (AD.nodes diag)
      st_edges_petri = foldr insertEdge st_petri (AD.connections diag)
      st_support_petri = foldr addSupportST st_edges_petri (M.keys $ nodes st_edges_petri)
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

addSupportST :: Net p n => PetriKey -> p n PetriKey -> p n PetriKey
addSupportST sourceKey petri =
  let sourceNode = nodes petri M.! sourceKey
      fn = if isPlaceNode sourceNode then isPlaceNode else isTransitionNode
      nodesToBeFixed = M.filter fn
        $ M.mapMaybeWithKey (\k _ -> M.lookup k (nodes petri))
        $ outFlow sourceKey petri
  in M.foldrWithKey (addSupportST' sourceKey) petri nodesToBeFixed

addSupportST'
  :: Net p n
  => PetriKey
  -> PetriKey
  -> n PetriKey
  -> p n PetriKey
  -> p n PetriKey
addSupportST' sourceKey targetKey targetNode petri =
  let supportKey = SupportST {label = (+ 1) $ maximum $ map label $ M.keys $ nodes petri}
  in repsertFlow supportKey 1 targetKey
     . repsertFlow sourceKey 1 supportKey
     . repsertNode supportKey (if isPlaceNode targetNode then Nothing else Just 0)
     . deleteFlow sourceKey targetKey
     $ petri

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
  :: Net p n
  => AD.ADConnection
  -> p n PetriKey
  -> p n PetriKey
insertEdge edge petri = fromMaybe petri $ do
  sourceKey <- find (\k -> label k == AD.from edge) $ M.keys $ nodes petri
  targetKey <- find (\k -> label k == AD.to edge) $ M.keys $ nodes petri
  return $ repsertFlow sourceKey 1 targetKey petri
