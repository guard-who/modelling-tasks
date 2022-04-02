module AD_Datatype (
  ADConnection(..),
  ADNode(..),
  UMLActivityDiagram(..)
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
  | ADActivityEndNode {
      label :: Int
  }
  | ADFlowEndNode {
      label :: Int
  }
  | ADStartNode {
      label :: Int
  } deriving (Show, Eq)


data UMLActivityDiagram =
  UMLActivityDiagram {
    nodes :: [ADNode],
    connections :: [ADConnection]
  } deriving (Show, Eq)