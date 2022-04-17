{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

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
--TODO: Handling fork/if-structures (finding a fork/decision node -> case distinction -> find corresponding merge -> traverse all paths to it)
convertNode' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
convertNode' [] _ _ = ""
convertNode' (current:queue) diag seen = 
  let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
      newSeen = seen ++ [current]
  in case current of 
        ADActionNode {name} -> [i|:#{name};\n|] ++ convertNode' newQueue diag newSeen
        ADObjectNode {name} -> [i|:#{name}]\n|] ++ convertNode' newQueue diag newSeen
        ADInitialNode {} -> "start\n" ++ convertNode' newQueue diag newSeen
        ADActivityFinalNode {} -> "end\n" ++ convertNode' newQueue diag newSeen
        ADFlowFinalNode {} -> "stop\n" ++ convertNode' newQueue diag newSeen
        ADMergeNode {} -> "repeat\n" ++ handleRepeat current convertNode' newQueue diag newSeen
        _ -> ""


--Might be better to get repeat-structures directly from the alloy instance, but should work for now
--Traverses path until a decision node pointing towards the merge node is found, which is the corresponding repeatEnd
handleRepeat :: ADNode -> ([ADNode] -> UMLActivityDiagram -> [ADNode] -> String) -> [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
handleRepeat _ _ [] _ _ = ""
handleRepeat mergenode callback (current:queue) diag seen =
    let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
        newSeen = seen ++ [current]
    in case current of 
        ADActionNode {name} -> [i|:#{name};\n|] ++ handleRepeat mergenode callback newQueue diag newSeen
        ADObjectNode {name} -> [i|:#{name}]\n|] ++ handleRepeat mergenode callback newQueue diag newSeen
        ADInitialNode {} -> "start\n" ++ handleRepeat mergenode callback newQueue diag newSeen
        ADActivityFinalNode {} -> "end\n" ++ handleRepeat mergenode callback newQueue diag newSeen
        ADFlowFinalNode {} -> "stop\n" ++ handleRepeat mergenode callback newQueue diag newSeen
        ADMergeNode {} -> "repeat\n" ++ handleRepeat current (handleRepeat mergenode callback) newQueue diag newSeen
        ADDecisionNode {} -> if mergenode `elem` adjNodes current diag then "repeat while ()\n" ++ callback newQueue diag newSeen
                             else "" -- TODO
        _ -> "" 