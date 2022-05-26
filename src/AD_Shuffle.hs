{-# LANGUAGE NamedFieldPuns #-}

module AD_Shuffle (
  shuffleAD, shufflePetri
) where

import qualified Data.Map as M ((!), fromList)
import qualified AD_Datatype as AD (ADNode(label))

import AD_Datatype (
  UMLActivityDiagram(..),
  ADNode (..),
  ADConnection (..),
  isActionNode, isObjectNode)
import AD_Petrinet (PetriKey(..))

import Modelling.PetriNet.Types(PetriLike(..))

import Data.Map (Map)
import System.Random (mkStdGen)
import System.Random.Shuffle(shuffle')


shuffleAD :: Int -> UMLActivityDiagram -> UMLActivityDiagram
shuffleAD seed diag = shuffleADNames seed $ shuffleADLabels seed diag


shuffleADLabels :: Int -> UMLActivityDiagram -> UMLActivityDiagram
shuffleADLabels seed diag =
  let labels = map AD.label $ nodes diag
      relabeling = M.fromList $ zip labels $ shuffle' labels (length labels) (mkStdGen seed)
      newNodes = map (updateNodeLabel relabeling) $ nodes diag
      newConnections = map (updateConnection relabeling) $ connections diag
  in UMLActivityDiagram {nodes=newNodes, connections=newConnections}

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


shuffleADNames :: Int -> UMLActivityDiagram -> UMLActivityDiagram
shuffleADNames seed UMLActivityDiagram{nodes, connections} =
  let names = map name $ filter (\n -> isActionNode n || isObjectNode n) nodes
      renaming = M.fromList $ zip names $ shuffle' names (length names) (mkStdGen seed)
      newNodes = map (updateName renaming) nodes
  in UMLActivityDiagram {nodes=newNodes, connections=connections}

updateName :: Map String String -> ADNode -> ADNode
updateName renaming node =
  case node of
    ADActionNode {label, name} -> ADActionNode {label=label, name=rename name}
    ADObjectNode {label, name} -> ADObjectNode {label=label, name=rename name}
    _ -> node
  where rename s = renaming M.! s

--To be implemented
shufflePetri :: Int -> PetriLike PetriKey -> PetriLike PetriKey
shufflePetri _ petri = petri