{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.Shuffle (
  shuffleADLabels, shuffleADNames, shufflePetri
) where

import qualified Data.Map as M ((!), fromList, keys)
import qualified Modelling.ActivityDiagram.Datatype as AD (ADNode(label))
import qualified Modelling.ActivityDiagram.Petrinet as PK (PetriKey(label))
import qualified Modelling.PetriNet.Types as PN (
  Net (..)
  )

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode (..),
  ADConnection (..),
  isActionNode, isObjectNode)

import Modelling.ActivityDiagram.Petrinet (PetriKey(..))
import Modelling.PetriNet.Types (
  Net,
  )

import Control.Monad.Random (MonadRandom)
import Data.Map (Map)
import System.Random.Shuffle (shuffleM)


shuffleADLabels
  :: (MonadRandom m)
  => UMLActivityDiagram
  -> m (Map Int Int, UMLActivityDiagram)
shuffleADLabels diag = do
  let labels = map AD.label $ nodes diag
  relabeling <- M.fromList <$> zip labels <$> shuffleM labels
  let newNodes = map (updateNodeLabel relabeling) $ nodes diag
      newConnections = map (updateConnection relabeling) $ connections diag
  return (relabeling, UMLActivityDiagram {nodes=newNodes, connections=newConnections})

updateNodeLabel :: Map Int Int -> ADNode -> ADNode
updateNodeLabel relabeling node =
  case node of
    ADActionNode {label, name} -> ADActionNode {label=relabel label, name=name}
    ADObjectNode {label, name} -> ADObjectNode {label=relabel label, name=name}
    ADDecisionNode {label} -> ADDecisionNode {label=relabel label}
    ADMergeNode {label} -> ADMergeNode {label=relabel label}
    ADForkNode {label} -> ADForkNode {label=relabel label}
    ADJoinNode {label} -> ADJoinNode {label=relabel label}
    ADInitialNode {label} -> ADInitialNode {label=relabel label}
    ADActivityFinalNode {label} -> ADActivityFinalNode {label=relabel label}
    ADFlowFinalNode {label} -> ADFlowFinalNode {label=relabel label}
  where relabel n = relabeling M.! n

updateConnection :: Map Int Int -> ADConnection -> ADConnection
updateConnection relabeling ADConnection {from, to, guard} =
  let newFrom = relabeling M.! from
      newTo = relabeling M.! to
  in ADConnection {from=newFrom, to=newTo, guard=guard}


shuffleADNames
  :: (MonadRandom m)
  => UMLActivityDiagram
  -> m (Map String String, UMLActivityDiagram)
shuffleADNames UMLActivityDiagram{nodes, connections} = do
  let names = map name $ filter (\n -> isActionNode n || isObjectNode n) nodes
  renaming <- M.fromList <$> zip names <$> shuffleM names
  let newNodes = map (updateName renaming) nodes
  return (renaming, UMLActivityDiagram {nodes=newNodes, connections=connections})

updateName :: Map String String -> ADNode -> ADNode
updateName renaming node =
  case node of
    ADActionNode {label, name} -> ADActionNode {label=label, name=rename name}
    ADObjectNode {label, name} -> ADObjectNode {label=label, name=rename name}
    _ -> node
  where rename s = renaming M.! s


shufflePetri
  :: (MonadRandom m, Net p n)
  => p n PetriKey
  -> m (Map Int Int, p n PetriKey)
shufflePetri petri = do
  let labels = map PK.label $ M.keys $ PN.nodes petri
  relabeling <- M.fromList <$> zip labels <$> shuffleM labels
  return (relabeling, PN.mapNet (updatePetriKey relabeling) petri)

updatePetriKey :: Map Int Int -> PetriKey -> PetriKey
updatePetriKey relabeling key =
  case key of
    NormalST {label, sourceNode} -> NormalST {label=relabel label, sourceNode=sourceNode}
    SupportST {label} -> SupportST {label=relabel label}
  where relabel n = relabeling M.! n
