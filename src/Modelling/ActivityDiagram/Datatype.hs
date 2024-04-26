{-# LANGUAGE DeriveGeneric #-}
module Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram(..),
  getInitialNodes,
  getFinalNodes,
  isActionNode,
  isObjectNode,
  isDecisionNode,
  isMergeNode,
  isForkNode,
  isJoinNode,
  isInitialNode,
  isActivityFinalNode,
  isFlowFinalNode,
  adjNodes
) where

import GHC.Generics (Generic)

data AdConnection =
  AdConnection {
    from :: Int,
    to :: Int,
    guard :: String
  } deriving (Generic, Read, Show, Eq, Ord)

data AdNode =
  AdActionNode {
    label :: Int,
    name :: String
  }
  | AdObjectNode {
      label :: Int,
      name :: String
  }
  | AdDecisionNode {
      label :: Int
  }
  | AdMergeNode {
      label :: Int
  }
  | AdForkNode {
      label :: Int
  }
  | AdJoinNode {
      label :: Int
  }
  | AdActivityFinalNode {
      label :: Int
  }
  | AdFlowFinalNode {
      label :: Int
  }
  | AdInitialNode {
      label :: Int
  } deriving (Generic, Read, Show, Eq)


data UMLActivityDiagram =
  UMLActivityDiagram {
    nodes :: [AdNode],
    connections :: [AdConnection]
  } deriving (Generic, Read, Show, Eq)


adjNodes :: AdNode -> UMLActivityDiagram -> [AdNode]
adjNodes node (UMLActivityDiagram ns conns) =
  let adjLabel = map to $ filter (\c -> from c == label node) conns
  in filter (\node' -> label node' `elem` adjLabel) ns

isActionNode :: AdNode -> Bool
isActionNode node =
  case node of
    AdActionNode {} -> True
    _ -> False

isObjectNode :: AdNode -> Bool
isObjectNode node =
  case node of
    AdObjectNode {} -> True
    _ -> False

isDecisionNode :: AdNode -> Bool
isDecisionNode node =
  case node of
    AdDecisionNode {} -> True
    _ -> False

isMergeNode :: AdNode -> Bool
isMergeNode node =
  case node of
    AdMergeNode {} -> True
    _ -> False

isForkNode :: AdNode -> Bool
isForkNode node =
  case node of
    AdForkNode {} -> True
    _ -> False

isJoinNode :: AdNode -> Bool
isJoinNode node =
  case node of
    AdJoinNode {} -> True
    _ -> False

isInitialNode :: AdNode -> Bool
isInitialNode node =
  case node of
    AdInitialNode {} -> True
    _ -> False

isActivityFinalNode :: AdNode -> Bool
isActivityFinalNode node =
  case node of
    AdActivityFinalNode {} -> True
    _ -> False

isFlowFinalNode :: AdNode -> Bool
isFlowFinalNode node =
  case node of
    AdFlowFinalNode {} -> True
    _ -> False


getInitialNodes :: UMLActivityDiagram -> [AdNode]
getInitialNodes (UMLActivityDiagram ns _) =
  filter isInitialNode ns

getFinalNodes :: UMLActivityDiagram -> [AdNode]
getFinalNodes (UMLActivityDiagram ns _) =
  filter (\x -> isActivityFinalNode x || isFlowFinalNode x) ns
