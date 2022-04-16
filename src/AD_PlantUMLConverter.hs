{-# LANGUAGE QuasiQuotes #-}

module AD_PlantUMLConverter (
  convertToPlantUML
) where

import Data.String.Interpolate ( i )

import AD_Datatype (
  ADNode(..),
  UMLActivityDiagram(..),
  getInitialNodes,
  adjNodes,
  )

convertToPlantUML :: UMLActivityDiagram -> String
convertToPlantUML diag = 
    let start = getInitialNodes diag
        body = convertNode start diag
    in "@startuml\n" ++ body ++ "@enduml"

convertNode :: [ADNode] -> UMLActivityDiagram -> String
convertNode queue diag = convertNode' queue diag []

--Use BFS to traverse the graph
convertNode' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
convertNode' [] _ _ = ""
convertNode' (current:queue) diag seen = 
  let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
      newSeen = seen ++ [current]
  in case current of 
        ADActionNode {name = s} -> [i|:#{s};\n|] ++ convertNode' newQueue diag newSeen
        ADObjectNode {name = s} -> [i|:#{s}]\n|] ++ convertNode' newQueue diag newSeen
        ADInitialNode {} -> "start\n" ++ convertNode' newQueue diag newSeen
        ADActivityFinalNode {} -> "end\n" ++ convertNode' newQueue diag newSeen
        ADFlowFinalNode {} -> "stop\n" ++ convertNode' newQueue diag newSeen
        _ -> ""