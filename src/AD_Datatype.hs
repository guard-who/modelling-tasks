module AD_Datatype (
  ADConnection(..),
  ADNodes(..),
  UMLActivityDiagram(..)
) where 

data ADConnection = ADConnection Integer Integer String deriving (Show, Eq)

data ADNodes =
  ActionNode {
    label :: Integer,
    name :: String
  }
  | ObjectNode {
      label :: Integer,
      name :: String
  } 
  | DecisionNode {
      label :: Integer
  } 
  | MergeNode {
      label :: Integer
  }
  | ForkNode {
      label :: Integer
  }
  | JoinNode {
      label :: Integer
  }
  | ActivityEndNode {
      label :: Integer
  }
  | FlowEndNode {
      label :: Integer
  }
  | StartNode {
      label :: Integer
  } deriving (Show, Eq)


data UMLActivityDiagram =
  UMLActivityDiagram {
    nodes :: [ADNodes],
    connections :: [ADConnection],
  } deriving (Show, Eq)