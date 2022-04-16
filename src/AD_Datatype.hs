module AD_Datatype (
  ADConnection(..),
  ADNode(..),
  UMLActivityDiagram(..),
  getInitialNodes,
  adjNodes
) where 

data ADConnection = ADConnection Int Int String deriving (Show, Eq, Ord)

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
  let adjLabel = map getTo $ filter (\c -> getFrom c == getLabel node) conns
  in filter (\node' -> getLabel node' `elem` adjLabel) ns


getInitialNodes :: UMLActivityDiagram -> [ADNode]
getInitialNodes (UMLActivityDiagram ns _) = 
  filter isInitialNode ns
  where isInitialNode ADInitialNode {} = True
        isInitialNode _ = False

getFrom :: ADConnection -> Int 
getFrom (ADConnection l _ _) = l

getTo :: ADConnection -> Int
getTo (ADConnection _ l _) = l

getLabel :: ADNode -> Int
getLabel = label