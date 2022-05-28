module AD_Datatype (
  ADConnection(..),
  ADNode(..),
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

data ADConnection =
  ADConnection {
    from :: Int,
    to :: Int,
    guard :: String
  } deriving (Show, Eq, Ord)

data ADNode =
  ADActionNode {
    label :: Int,
    name :: String
  }
  | ADObjectNode {
      label :: Int,
      name :: String
  }
  | ADDecisionNode {
      label :: Int
  }
  | ADMergeNode {
      label :: Int
  }
  | ADForkNode {
      label :: Int
  }
  | ADJoinNode {
      label :: Int
  }
  | ADActivityFinalNode {
      label :: Int
  }
  | ADFlowFinalNode {
      label :: Int
  }
  | ADInitialNode {
      label :: Int
  } deriving (Show, Eq)


data UMLActivityDiagram =
  UMLActivityDiagram {
    nodes :: [ADNode],
    connections :: [ADConnection]
  } deriving (Show, Eq)


adjNodes :: ADNode -> UMLActivityDiagram -> [ADNode]
adjNodes node (UMLActivityDiagram ns conns) =
  let adjLabel = map to $ filter (\c -> from c == label node) conns
  in filter (\node' -> label node' `elem` adjLabel) ns

isActionNode :: ADNode -> Bool
isActionNode node =
  case node of
    ADActionNode {} -> True
    _ -> False

isObjectNode :: ADNode -> Bool
isObjectNode node =
  case node of
    ADObjectNode {} -> True
    _ -> False

isDecisionNode :: ADNode -> Bool
isDecisionNode node =
  case node of
    ADDecisionNode {} -> True
    _ -> False

isMergeNode :: ADNode -> Bool
isMergeNode node =
  case node of
    ADMergeNode {} -> True
    _ -> False

isForkNode :: ADNode -> Bool
isForkNode node =
  case node of
    ADForkNode {} -> True
    _ -> False

isJoinNode :: ADNode -> Bool
isJoinNode node =
  case node of
    ADJoinNode {} -> True
    _ -> False

isInitialNode :: ADNode -> Bool
isInitialNode node =
  case node of
    ADInitialNode {} -> True
    _ -> False

isActivityFinalNode :: ADNode -> Bool
isActivityFinalNode node =
  case node of
    ADActivityFinalNode {} -> True
    _ -> False

isFlowFinalNode :: ADNode -> Bool
isFlowFinalNode node =
  case node of
    ADFlowFinalNode {} -> True
    _ -> False


getInitialNodes :: UMLActivityDiagram -> [ADNode]
getInitialNodes (UMLActivityDiagram ns _) =
  filter isInitialNode ns

getFinalNodes :: UMLActivityDiagram -> [ADNode]
getFinalNodes (UMLActivityDiagram ns _) =
  filter (\x -> isActivityFinalNode x || isFlowFinalNode x) ns