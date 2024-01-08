{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUMLConvConf(..),
  defaultPlantUMLConvConf,
  convertToPlantUML,
  convertToPlantUML',
  drawADToFile
) where

import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.List ( delete, intercalate, intersect, union )
import Data.String.Interpolate ( i, __i )
import GHC.Generics (Generic)
import Language.PlantUML.Call (DiagramType(SVG), drawPlantUMLDiagram)

import Modelling.ActivityDiagram.Datatype (
  ADNode(..),
  UMLActivityDiagram(..),
  getInitialNodes,
  adjNodes,
  )

data PlantUMLConvConf = PlantUMLConvConf {
  suppressNodeNames :: Bool,
  suppressBranchConditions :: Bool
} deriving (Generic, Read, Show, Eq)

defaultPlantUMLConvConf :: PlantUMLConvConf
defaultPlantUMLConvConf = PlantUMLConvConf {
  suppressNodeNames = False,
  suppressBranchConditions = False
}

drawADToFile :: FilePath -> PlantUMLConvConf -> UMLActivityDiagram -> IO FilePath
drawADToFile path conf ad = do
  svg <- drawPlantUMLDiagram SVG $ convertToPlantUML' conf ad
  B.writeFile adFilename svg
  return adFilename
  where
    adFilename :: FilePath
    adFilename = [i|#{path}Diagram.svg|]

convertToPlantUML :: UMLActivityDiagram -> ByteString
convertToPlantUML = convertToPlantUML' defaultPlantUMLConvConf

convertToPlantUML' :: PlantUMLConvConf -> UMLActivityDiagram -> ByteString
convertToPlantUML' conf diag =
    let start = getInitialNodes diag
        body = convertNode start conf diag
        document = "@startuml\n" ++ body ++ "@enduml"
    in [__i|#{document}|]

convertNode :: [ADNode] -> PlantUMLConvConf -> UMLActivityDiagram -> String
convertNode queue conf diag = convertNode' queue conf diag []

--Traverse the graph and serialize the nodes along the way to a PlantUML-String
convertNode' :: [ADNode] -> PlantUMLConvConf -> UMLActivityDiagram -> [ADNode] -> String
convertNode' [] _ _ _ = [__i||]
convertNode' (current:queue) conf@(PlantUMLConvConf sn sb) diag seen =
  let newQueue = filter (`notElem` seen) (queue ++ adjNodes current diag)
      newSeen = seen ++ [current]
  in case current of
        ADActionNode {name} ->
          [__i|:#{f sn name};
          #{convertNode' newQueue conf diag newSeen}|]
        ADObjectNode {name} ->
          [__i|:#{f sn name}]
          #{convertNode' newQueue conf diag newSeen}|]
        ADInitialNode {} ->
          [__i|start
          #{convertNode' newQueue conf diag newSeen}|]
        ADActivityFinalNode {} ->
          [__i|stop\n|]
        ADFlowFinalNode {} ->
          [__i|end\n|]
        ADMergeNode {} ->
          [__i|#{handleRepeat current conf diag newSeen repeatStart repeatEnd}|]
        ADDecisionNode {} ->
          [__i|#{handleDecisionOrFork current conf diag newSeen ifElseStart ifElseMid ifElseEnd}|]
        ADForkNode {} ->
          [__i|#{handleDecisionOrFork current conf diag newSeen forkStart forkMid forkEnd}|]
        _ -> [__i||]
  where
    f p x = if p then "        " else x
    repeatStart = "repeat"
    repeatEnd = [__i|repeat while () #{f sb "is ([Y]) not ([X])"}|]
    ifElseStart = [__i|if () then #{f sb "([X])"}|]
    ifElseMid = [__i|else #{f sb "([Y])"}\n|]
    ifElseEnd = "endif"
    forkStart = "fork"
    forkMid = "forkagain\n"
    forkEnd = "forkend"


--Strategy: Find the corresponding merge/join to the decision/fork: That should be the first node reachable from all decision paths
--This should be the the head of the intersection of nodes traversed from the nodes that the decision/fork points to
--Then: Determine the subpaths between the decision and the merge and handle them via convertNode'
handleDecisionOrFork :: ADNode -> PlantUMLConvConf -> UMLActivityDiagram -> [ADNode] -> String -> String -> String -> String
handleDecisionOrFork startNode conf diag@(UMLActivityDiagram _ conns) seen startToken midToken endToken =
  let endNode = head $ foldr1 intersect $ filterDisjunctSublists $ map (\x -> traverseFromNode x diag seen) $ adjNodes startNode diag
      pathsToEnd =
        map
        (filter (\ x -> x `notElem` traverseFromNode endNode diag seen)
        . (\ x -> traverseFromNode x diag seen))
        (adjNodes startNode diag)
      subDiags = map (`UMLActivityDiagram` conns) pathsToEnd
      subStrings = map (\xs -> convertNode' [head $ nodes xs] conf xs seen) subDiags
      newSeen = seen ++ foldr1 union pathsToEnd ++ [endNode]
      newQueue = filter (`notElem` newSeen) (adjNodes endNode diag)
  in
    [__i|
    #{startToken}
    #{intercalate midToken subStrings}#{endToken}
    #{convertNode' newQueue conf diag newSeen}
    |]


-- Filter out sublists that are disjunct with all other sublists
filterDisjunctSublists :: (Eq a) => [[a]] -> [[a]]
filterDisjunctSublists ws = filterDisjunctSublists' ws ws
  where filterDisjunctSublists' sublists = foldr (\hs js -> if allDisjunctWith hs sublists then js
                                                            else hs:js) []
        allDisjunctWith xs ys = all (disjunct xs) (delete xs ys)
        disjunct xs = null . intersect xs

--Strategy: Find the corresponding decision to the merge: That should be the node reachable from the merge (but not traversed yet) that has an edge towards it
--Then: Determine the subpath between the merge and the decision and handle them via convertNode'
handleRepeat :: ADNode -> PlantUMLConvConf -> UMLActivityDiagram -> [ADNode] -> String -> String -> String
handleRepeat merge conf diag@(UMLActivityDiagram _ conns) seen startToken endToken =
  let repeatEnd =  head $ dropWhile (\x -> merge `notElem` adjNodes x diag) $ traverseFromNode merge diag seen
      pathToRepeatEnd = tail $ filter (\x -> x `notElem` traverseFromNode repeatEnd diag seen) $ traverseFromNode merge diag seen
      subString = convertNode' (adjNodes merge diag) conf (UMLActivityDiagram pathToRepeatEnd conns) seen
      newSeen = seen ++ pathToRepeatEnd ++ [repeatEnd]
      newQueue = filter (`notElem` newSeen) (adjNodes repeatEnd diag)
  in
    [__i|
    #{startToken}
    #{subString}#{endToken}
    #{convertNode' newQueue conf diag newSeen}
    |]


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
