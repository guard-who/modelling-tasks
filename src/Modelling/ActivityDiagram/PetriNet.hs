{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.PetriNet (
  PetriKey (..),
  convertToPetriNet,
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

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..)
  )

import Modelling.PetriNet.Types (
  Net (..),
  PetriNode (..),
  SimplePetriLike,
  )

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (find)
import GHC.Generics (Generic)
import Modelling.ActivityDiagram.Datatype (
  isActivityFinalNode, isFlowFinalNode
  )


data PetriKey
  = SupportPetriNode {label :: Int}
  | NormalPetriNode {label :: Int, sourceNode :: Ad.AdNode}
  deriving (Generic, Eq, Read, Show)

instance Ord PetriKey where
  pk1 `compare` pk2 = label pk1 `compare` label pk2

convertToSimple :: Ad.UMLActivityDiagram -> SimplePetriLike PetriKey
convertToSimple = convertToPetriNet

convertToPetriNet :: Net p n => Ad.UMLActivityDiagram -> p n PetriKey
convertToPetriNet diag =
  let st_petri = foldr insertNode emptyNet (Ad.nodes diag)
      st_edges_petri = foldr insertEdge st_petri (Ad.connections diag)
      st_support_petri = foldr
        addSupportPetriNode
        st_edges_petri
        (M.keys $ nodes st_edges_petri)
  in relabelPetri $ removeFinalPlaces st_support_petri

-- Relabels Petri net nodes in order to avoid "missing" numbers resulting from the creation of sink transitions
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
    NormalPetriNode {label, sourceNode} ->
      NormalPetriNode {label = relabel label, sourceNode = sourceNode}
    SupportPetriNode {label} -> SupportPetriNode {label = relabel label}
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
    NormalPetriNode {sourceNode} ->
      if isActivityFinalNode sourceNode || isFlowFinalNode sourceNode then
         deleteNode key petri
      else petri
    _ -> petri

addSupportPetriNode :: Net p n => PetriKey -> p n PetriKey -> p n PetriKey
addSupportPetriNode sourceKey petri =
  let sourceNode = nodes petri M.! sourceKey
      fn = if isPlaceNode sourceNode then isPlaceNode else isTransitionNode
      nodesToBeFixed = M.filter fn
        $ M.mapMaybeWithKey (\k _ -> M.lookup k (nodes petri))
        $ outFlow sourceKey petri
  in M.foldrWithKey (addSupportPetriNode' sourceKey) petri nodesToBeFixed

addSupportPetriNode'
  :: Net p n
  => PetriKey
  -> PetriKey
  -> n PetriKey
  -> p n PetriKey
  -> p n PetriKey
addSupportPetriNode' sourceKey targetKey targetNode petri =
  let supportKey = SupportPetriNode {
        label = (+ 1) $ maximum $ map label $ M.keys $ nodes petri
        }
  in alterFlow supportKey 1 targetKey
     . alterFlow sourceKey 1 supportKey
     . alterNode supportKey (if isPlaceNode targetNode then Nothing else Just 0)
     . deleteFlow sourceKey targetKey
     $ petri

insertNode
  :: Net p n
  => Ad.AdNode
  -> p n PetriKey
  -> p n PetriKey
insertNode =
  uncurry alterNode . nodeToPetriNode

nodeToPetriNode :: Ad.AdNode -> (PetriKey, Maybe Int)
nodeToPetriNode node =
  case node of
    Ad.AdInitialNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 1
      )
    Ad.AdActionNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Nothing
      )
    Ad.AdObjectNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 0
      )
    Ad.AdDecisionNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 0
      )
    Ad.AdMergeNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 0
      )
    Ad.AdForkNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Nothing
      )
    Ad.AdJoinNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Nothing
      )
    Ad.AdActivityFinalNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 0
      )
    Ad.AdFlowFinalNode {label} -> (
      NormalPetriNode {label = label, sourceNode = node},
      Just 0
      )

insertEdge
  :: Net p n
  => Ad.AdConnection
  -> p n PetriKey
  -> p n PetriKey
insertEdge edge petri = fromMaybe petri $ do
  sourceKey <- find (\k -> label k == Ad.from edge) $ M.keys $ nodes petri
  targetKey <- find (\k -> label k == Ad.to edge) $ M.keys $ nodes petri
  return $ alterFlow sourceKey 1 targetKey petri
