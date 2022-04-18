{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module AD_PlantUMLConverter (
  convertToPlantUML
) where

import Data.List
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

--Traverse the graph and serialize the nodes along the way to a PlantUML-String
--TODO: Handling fork structures 
convertNode' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
convertNode' [] _ _ = ""
convertNode' (current:queue) diag seen = 
  let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
      newSeen = seen ++ [current]
  in case current of 
        ADActionNode {name} -> [i|:#{name};\n|] ++ convertNode' newQueue diag newSeen
        ADObjectNode {name} -> [i|:#{name}]\n|] ++ convertNode' newQueue diag newSeen
        ADInitialNode {} -> "start\n" ++ convertNode' newQueue diag newSeen
        ADActivityFinalNode {} -> "end\n" 
        ADFlowFinalNode {} -> "stop\n" 
        ADMergeNode {} -> "repeat\n" ++ handleRepeat current newQueue diag newSeen
        ADDecisionNode {} -> "if () then ([X])\n" ++ handleDecision current newQueue diag newSeen
        ADForkNode {} -> "fork\n" ++ handleFork current newQueue diag newSeen
        _ -> ""


--Strategy: Find the corresponding merge to the decision: That should be the first node reachable from all decision paths
--Since BFS returns nodes in order of their distance, the head of the intersection of the BFS-Path-Lists should be that node
--Then: Determine the subpaths between the decision and the merge and handle them via convertNode'
handleDecision :: ADNode -> [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
handleDecision decision _ diag@(UMLActivityDiagram _ conns) seen =
  let ifElseEnd = head $ foldr1 intersect $ map (\x -> traverseFromNodes [x] diag [] seen) $ adjNodes decision diag
      pathsToIfElseEnd = map (\xs -> filter (\x -> x `notElem` traverseFromNodes [ifElseEnd] diag [] seen) xs) 
        $ map (\x -> traverseFromNodes [x] diag [] seen) $ adjNodes decision diag
      subDiags = map (\xs -> UMLActivityDiagram xs conns) pathsToIfElseEnd
      subStrings = map (\xs -> convertNode' [head $ nodes xs] xs seen) subDiags
      newSeen = seen ++ (foldr1 union pathsToIfElseEnd) ++ [ifElseEnd]
      newQueue = filter (`notElem` newSeen) (adjNodes ifElseEnd diag)
  in (intercalate "else ([Y])\n" subStrings) ++ "endif\n" ++ convertNode' newQueue diag newSeen


--Same as handleDecision except for some tokens, TODO: pass those as parameters
handleFork :: ADNode -> [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
handleFork decision _ diag@(UMLActivityDiagram _ conns) seen =
  let ifElseEnd = head $ foldr1 intersect $ map (\x -> traverseFromNodes [x] diag [] seen) $ adjNodes decision diag
      pathsToIfElseEnd = map (\xs -> filter (\x -> x `notElem` traverseFromNodes [ifElseEnd] diag [] seen) xs) 
        $ map (\x -> traverseFromNodes [x] diag [] seen) $ adjNodes decision diag
      subDiags = map (\xs -> UMLActivityDiagram xs conns) pathsToIfElseEnd
      subStrings = map (\xs -> convertNode' [head $ nodes xs] xs seen) subDiags
      newSeen = seen ++ (foldr1 union pathsToIfElseEnd) ++ [ifElseEnd]
      newQueue = filter (`notElem` newSeen) (adjNodes ifElseEnd diag)
  in (intercalate "forkagain\n" subStrings) ++ "forkend\n" ++ convertNode' newQueue diag newSeen


--Strategy: Find the corresponding decision to the merge: That should be the node reachable from the merge (but not traversed yet) that has an edge towards it 
--Then: Determine the subpath between the merge and the decision and handle them via convertNode'
handleRepeat :: ADNode -> [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
handleRepeat merge queue diag@(UMLActivityDiagram _ conns) seen =
  let repeatEnd =  head $ dropWhile (\x -> merge `notElem` adjNodes x diag) $ traverseFromNodes [merge] diag [] seen
      pathToRepeatEnd = tail $ filter (\x -> x `notElem` traverseFromNodes [repeatEnd] diag [] seen) $ traverseFromNodes [merge] diag [] seen
      subString = convertNode' queue (UMLActivityDiagram pathToRepeatEnd conns) seen
      newSeen = seen ++ pathToRepeatEnd ++ [repeatEnd] 
      newQueue = filter (`notElem` newSeen) (adjNodes repeatEnd diag)
  in subString ++ "repeat while () is ([Y]) not ([X])\n" ++ convertNode' newQueue diag newSeen


--Crude implementation of BFS, in order to get a list of reachable (yet unhandled) nodes 
traverseFromNodes :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> [ADNode] -> [ADNode]
traverseFromNodes [] _ path _ = path
traverseFromNodes (current:queue) diag path seen =
  let newQueue = queue ++ filter (`notElem` (path ++ seen)) (adjNodes current diag)
      newPath = if current `elem` (path ++ seen) then path
                else path ++ [current]
  in traverseFromNodes newQueue diag newPath seen 