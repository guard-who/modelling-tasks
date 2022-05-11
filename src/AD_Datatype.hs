module AD_Datatype (
  ADConnection(..),
  ADNode(..),
  UMLActivityDiagram(..),
  getInitialNodes,
  getFinalNodes,
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


getInitialNodes :: UMLActivityDiagram -> [ADNode]
getInitialNodes (UMLActivityDiagram ns _) = 
  filter isInitialNode ns
  where isInitialNode ADInitialNode {} = True
        isInitialNode _ = False

getFinalNodes :: UMLActivityDiagram -> [ADNode]
getFinalNodes (UMLActivityDiagram ns _) =
  filter isFinalNode ns 
  where isFinalNode ADActivityFinalNode {} = True
        isFinalNode ADFlowFinalNode {} = True
        isFinalNode _ = False