module Output where

import Util
import Types (AssociationType(..), Connection(..), Syntax)
import Edges

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.FilePath (dropExtension)

connectionArrow :: Bool -> Connection -> [Attribute]
connectionArrow _          Inheritance =
  [arrowTo emptyArr]
connectionArrow printNames (Assoc Composition name from to isRed) =
  arrow Composition ++ [HeadLabel (mult to)]
  ++ [redColor | isRed] ++ [label name | printNames]
  ++ case from of
       (1, Just 1) -> []
       (0, Just 1) -> [TailLabel (mult from)]
       _           -> error $ "invalid composition multiplicity"
connectionArrow printNames (Assoc a name from to isRed) =
  arrow a ++ [TailLabel (mult from), HeadLabel (mult to)]
  ++ [redColor | isRed] ++ [label name | printNames]

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

label :: String -> Attribute
label = Label . toLabelValue

drawCdFromSyntax :: Bool -> Syntax -> FilePath -> GraphvizOutput -> IO ()
drawCdFromSyntax printNames syntax file format = do
  let (classes, associations) = syntax
  let classNames = map fst classes
  let theNodes = classNames
  let inhEdges = mapMaybe (\(from,mto) -> fmap (\to -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Inheritance)) mto) classes
  let classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
        where
          subs seen name
            | name `elem` seen = []
            | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((== Just name) . snd) classes)
  let assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  let assocEdges = map (\(a,n,m1,from,to,m2) -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Assoc a n m1 m2 (shouldBeRed from to classesWithSubclasses assocsBothWays))) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [toLabel l, shape BoxShape], fmtEdge = \(_,_,l) -> connectionArrow printNames l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

drawOdFromInstance :: Bool -> String -> FilePath -> GraphvizOutput -> IO ()
drawOdFromInstance printNames input file format = do
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
  let theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
  let theEdges = map ((\[from,v:"$0",to] -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), v:[])) . splitOn "->") $
                 filter (not . null) (splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine)))))
  let graph = undir (mkGraph (zip [0..] theNodes) theEdges) :: Gr String String
  let dotGraph = setDirectedness graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [underlinedLabel (firstLower l ++ " : " ++ takeWhile (/= '$') l), shape BoxShape], fmtEdge = \(_,_,l) -> [label l | printNames] }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output
