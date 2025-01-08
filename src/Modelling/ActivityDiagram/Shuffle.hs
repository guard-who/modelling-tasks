{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.Shuffle (
  shuffleAdLabels,
  shuffleAdNames,
  shufflePetri,
) where

import qualified Data.Map as M ((!), fromList, keys)
import qualified Modelling.ActivityDiagram.Datatype as Ad (AdNode (label))
import qualified Modelling.ActivityDiagram.PetriNet as PK (PetriKey(label))
import qualified Modelling.PetriNet.Types as PN (
  Net (..)
  )

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..),
  isActionNode, isObjectNode)

import Modelling.ActivityDiagram.PetriNet (PetriKey (..), updatePetriKey)
import Modelling.PetriNet.Types (
  Net,
  )

import Control.Monad.Random (MonadRandom)
import Data.Map (Map)
import System.Random.Shuffle (shuffleM)


shuffleAdLabels
  :: (MonadRandom m)
  => UMLActivityDiagram
  -> m (Map Int Int, UMLActivityDiagram)
shuffleAdLabels diag = do
  let labels = map Ad.label $ nodes diag
  relabeling <- M.fromList . zip labels <$> shuffleM labels
  let newNodes = map (updateNodeLabel relabeling) $ nodes diag
      newConnections = map (updateConnection relabeling) $ connections diag
  return (relabeling, UMLActivityDiagram {nodes=newNodes, connections=newConnections})

updateNodeLabel :: Map Int Int -> AdNode -> AdNode
updateNodeLabel relabeling node =
  case node of
    AdActionNode {label, name} -> AdActionNode {
      label = relabel label,
      name = name
      }
    AdObjectNode {label, name} -> AdObjectNode {
      label = relabel label,
      name = name
      }
    AdDecisionNode {label} -> AdDecisionNode {label = relabel label}
    AdMergeNode {label} -> AdMergeNode {label = relabel label}
    AdForkNode {label} -> AdForkNode {label = relabel label}
    AdJoinNode {label} -> AdJoinNode {label = relabel label}
    AdInitialNode {label} -> AdInitialNode {label = relabel label}
    AdActivityFinalNode {label} -> AdActivityFinalNode {label = relabel label}
    AdFlowFinalNode {label} -> AdFlowFinalNode {label = relabel label}
  where relabel n = relabeling M.! n

updateConnection :: Map Int Int -> AdConnection -> AdConnection
updateConnection relabeling AdConnection {from, to, guard} =
  let newFrom = relabeling M.! from
      newTo = relabeling M.! to
  in AdConnection {from=newFrom, to=newTo, guard=guard}


shuffleAdNames
  :: (MonadRandom m)
  => UMLActivityDiagram
  -> m (Map String String, UMLActivityDiagram)
shuffleAdNames UMLActivityDiagram{nodes, connections} = do
  let names = map name $ filter (\n -> isActionNode n || isObjectNode n) nodes
  renaming <- M.fromList . zip names <$> shuffleM names
  let newNodes = map (updateName renaming) nodes
  return (renaming, UMLActivityDiagram {nodes=newNodes, connections=connections})

updateName :: Map String String -> AdNode -> AdNode
updateName renaming node =
  case node of
    AdActionNode {label, name} -> AdActionNode {label=label, name=rename name}
    AdObjectNode {label, name} -> AdObjectNode {label=label, name=rename name}
    _ -> node
  where rename s = renaming M.! s


shufflePetri
  :: (MonadRandom m, Net p n)
  => p n PetriKey
  -> m (Map Int Int, p n PetriKey)
shufflePetri petri = do
  let labels = map PK.label $ M.keys $ PN.nodes petri
  relabeling <- M.fromList . zip labels <$> shuffleM labels
  return (relabeling, PN.mapNet (updatePetriKey relabeling) petri)
