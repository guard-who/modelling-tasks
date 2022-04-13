module AD_PlantUMLConverter (
  convertToPlantUML
) where

import AD_Datatype (
  ADConnection(ADConnection),
  ADNode(..),
  UMLActivityDiagram(..),
  )


convertToPlantUML :: UMLActivityDiagram -> String
convertToPlantUML diag@(UMLActivityDiagram ns _) = 
    let start = head  $ filter isInitialNode ns
        body = convertNode (Just start) diag
    in "@startuml\n" ++ body ++ "@enduml"

convertNode :: Maybe ADNode -> UMLActivityDiagram -> String
convertNode node diag =
  case node of
    Just ADActionNode {label = l} -> ":A;\n" ++ convertNode (adjNode diag l) diag
    Just ADObjectNode {label = l} -> ":O]\n" ++ convertNode (adjNode diag l) diag
    Just ADInitialNode {label = l} -> "start\n" ++ convertNode (adjNode diag l) diag
    Just ADActivityFinalNode {label = l} -> "end\n" ++ convertNode (adjNode diag l) diag
    Just ADFlowFinalNode {label = l} -> "stop\n" ++ convertNode (adjNode diag l) diag
    _ -> ""


adjNode :: UMLActivityDiagram -> Int -> Maybe ADNode
adjNode (UMLActivityDiagram ns conns) l = 
  let next = map to $ filter isAdjToLabel conns
  in if null next then Nothing
     else let adjnodes = filter (hasLabel (head next)) ns
          in if null adjnodes then Nothing
             else Just (head adjnodes)
  where isAdjToLabel conn = (from conn) == l

hasLabel :: Int -> ADNode -> Bool
hasLabel l node = (label node) == l

from :: ADConnection -> Int 
from (ADConnection l _ _) = l

to :: ADConnection -> Int
to (ADConnection _ l _) = l

isInitialNode :: ADNode -> Bool
isInitialNode ADInitialNode {} = True
isInitialNode _ = False