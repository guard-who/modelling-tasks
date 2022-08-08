{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.PlantUMLConverter (
  convertToPlantUML
) where

import Data.List
import Data.String.Interpolate ( i )

import Modelling.ActivityDiagram.Datatype (
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
convertNode' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> String
convertNode' [] _ _ = ""
convertNode' (current:queue) diag seen =
  let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
      newSeen = seen ++ [current]
  in case current of
        ADActionNode {name} -> [i|:#{name};\n|] ++ convertNode' newQueue diag newSeen
        ADObjectNode {name} -> [i|:#{name}]\n|] ++ convertNode' newQueue diag newSeen
        ADInitialNode {} -> "start\n" ++ convertNode' newQueue diag newSeen
        ADActivityFinalNode {} -> "stop\n"
        ADFlowFinalNode {} -> "end\n"
        ADMergeNode {} -> "repeat\n" ++ handleRepeat current diag newSeen
        ADDecisionNode {} -> "if () then ([X])\n" ++ handleDecisionOrFork current diag newSeen "else ([Y])\n" "endif\n"
        ADForkNode {} -> "fork\n" ++ handleDecisionOrFork current diag newSeen "forkagain\n" "forkend\n"
        _ -> ""


--Strategy: Find the corresponding merge/join to the decision/fork: That should be the first node reachable from all decision paths
--This should be the the head of the intersection of nodes traversed from the nodes that the decision/fork points to
--Then: Determine the subpaths between the decision and the merge and handle them via convertNode'
handleDecisionOrFork :: ADNode -> UMLActivityDiagram -> [ADNode] -> String -> String -> String
handleDecisionOrFork startNode diag@(UMLActivityDiagram _ conns) seen midToken endToken =
  let endNode = head $ foldr1 intersect $ filterDisjunctSublists $ map (\x -> traverseFromNode x diag seen) $ adjNodes startNode diag
      pathsToEnd =
        map
        (filter (\ x -> x `notElem` traverseFromNode endNode diag seen)
        . (\ x -> traverseFromNode x diag seen))
        (adjNodes startNode diag)
      subDiags = map (`UMLActivityDiagram` conns) pathsToEnd
      subStrings = map (\xs -> convertNode' [head $ nodes xs] xs seen) subDiags
      newSeen = seen ++ foldr1 union pathsToEnd ++ [endNode]
      newQueue = filter (`notElem` newSeen) (adjNodes endNode diag)
  in intercalate midToken subStrings ++ endToken ++ convertNode' newQueue diag newSeen


-- Filter out sublists that are disjunct with all other sublists
filterDisjunctSublists :: (Eq a) => [[a]] -> [[a]]
filterDisjunctSublists ws = filterDisjunctSublists' ws ws
  where filterDisjunctSublists' sublists = foldr (\hs js -> if allDisjunctWith hs sublists then js
                                                            else hs:js) []
        allDisjunctWith xs ys = all (disjunct xs) (delete xs ys)
        disjunct xs = null . intersect xs

--Strategy: Find the corresponding decision to the merge: That should be the node reachable from the merge (but not traversed yet) that has an edge towards it
--Then: Determine the subpath between the merge and the decision and handle them via convertNode'
handleRepeat :: ADNode -> UMLActivityDiagram -> [ADNode] -> String
handleRepeat merge diag@(UMLActivityDiagram _ conns) seen =
  let repeatEnd =  head $ dropWhile (\x -> merge `notElem` adjNodes x diag) $ traverseFromNode merge diag seen
      pathToRepeatEnd = tail $ filter (\x -> x `notElem` traverseFromNode repeatEnd diag seen) $ traverseFromNode merge diag seen
      subString = convertNode' (adjNodes merge diag) (UMLActivityDiagram pathToRepeatEnd conns) seen
      newSeen = seen ++ pathToRepeatEnd ++ [repeatEnd]
      newQueue = filter (`notElem` newSeen) (adjNodes repeatEnd diag)
  in subString ++ "repeat while () is ([Y]) not ([X])\n" ++ convertNode' newQueue diag newSeen


--Get reachable (yet unhandled) nodes from passed node
traverseFromNode :: ADNode -> UMLActivityDiagram -> [ADNode] -> [ADNode]
traverseFromNode node = traverseFromNode' [node] [node]

--Implementation of BFS taking already previously handled (seen) nodes in account
traverseFromNode' :: [ADNode] -> [ADNode] -> UMLActivityDiagram -> [ADNode] -> [ADNode]
traverseFromNode' [] traversed _ _ = traversed
traverseFromNode' (current:queue) traversed diag seen =
  let nextNodes = filter (`notElem` (traversed ++ seen)) (adjNodes current diag)
      newQueue = queue ++ nextNodes
      newTraversed = traversed ++ nextNodes
  in traverseFromNode' newQueue newTraversed diag seen