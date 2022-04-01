data Connection = Connection Integer Integer String 

data ActivityNodes =
    ActivityNode {
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


data UMLActitiyDiagram = 
    UMLActitiyDiagram {
        nodes :: [ActivityNodes],
        connections :: [Connection],
        startNodes :: [Integer]
    }