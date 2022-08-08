{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.Shuffle (
  shuffleADLabels, shuffleADNames, shufflePetri
) where

import qualified Data.Map as M ((!), fromList, keys)
import qualified Modelling.ActivityDiagram.Datatype as AD (ADNode(label))
import qualified Modelling.ActivityDiagram.Petrinet as PK (PetriKey(label))

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode (..),
  ADConnection (..),
  isActionNode, isObjectNode)

import Modelling.ActivityDiagram.Petrinet (PetriKey(..))
import Modelling.PetriNet.Types(PetriLike(..), mapPetriLike)

import Data.Map (Map)
import System.Random (mkStdGen)
import System.Random.Shuffle(shuffle')


shuffleADLabels :: Int -> UMLActivityDiagram -> (Map Int Int, UMLActivityDiagram)
shuffleADLabels seed diag =
  let labels = map AD.label $ nodes diag
      relabeling = M.fromList $ zip labels $ shuffle' labels (length labels) (mkStdGen seed)
      newNodes = map (updateNodeLabel relabeling) $ nodes diag
      newConnections = map (updateConnection relabeling) $ connections diag
  in (relabeling, UMLActivityDiagram {nodes=newNodes, connections=newConnections})

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


shuffleADNames :: Int -> UMLActivityDiagram -> (Map String String, UMLActivityDiagram)
shuffleADNames seed UMLActivityDiagram{nodes, connections} =
  let names = map name $ filter (\n -> isActionNode n || isObjectNode n) nodes
      renaming = M.fromList $ zip names $ shuffle' names (length names) (mkStdGen seed)
      newNodes = map (updateName renaming) nodes
  in (renaming, UMLActivityDiagram {nodes=newNodes, connections=connections})

updateName :: Map String String -> ADNode -> ADNode
updateName renaming node =
  case node of
    ADActionNode {label, name} -> ADActionNode {label=label, name=rename name}
    ADObjectNode {label, name} -> ADObjectNode {label=label, name=rename name}
    _ -> node
  where rename s = renaming M.! s


shufflePetri :: Int -> PetriLike PetriKey -> (Map Int Int, PetriLike PetriKey)
shufflePetri seed petri =
  let labels = map PK.label $ M.keys $ allNodes petri
      relabeling = M.fromList $ zip labels $ shuffle' labels (length labels) (mkStdGen seed)
  in (relabeling, mapPetriLike (updatePetriKey relabeling) petri)

updatePetriKey :: Map Int Int -> PetriKey -> PetriKey
updatePetriKey relabeling key =
  case key of
    NormalST {label} -> NormalST {label=relabel label}
    SupportST {label} -> SupportST {label=relabel label}
  where relabel n = relabeling M.! n