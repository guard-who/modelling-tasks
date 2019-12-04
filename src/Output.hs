module Output (drawCdFromSyntax, drawOdFromInstance) where

import qualified Data.Map as M (lookup)

import Auxiliary.Util
import Types (AssociationType(..), Connection(..), Syntax)
import Edges

import Data.List
import Data.List.Split
import Data.Map                          (Map)
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.Random.Shuffle (shuffleM)

import System.FilePath (dropExtension)

connectionArrow :: Bool -> Bool -> Maybe Attribute -> Connection -> [Attribute]
connectionArrow _ _ _ Inheritance =
  [arrowTo emptyArr]
connectionArrow _ printNames marking (Assoc Composition name from to isMarked) =
  arrow Composition ++ [HeadLabel (mult to)]
  ++ concat [maybeToList marking | isMarked] ++ [toLabel name | printNames]
  ++ case from of
       (1, Just 1) -> []
       (0, Just 1) -> [TailLabel (mult from)]
       _           -> error "invalid composition multiplicity"
connectionArrow printNavigations printNames marking (Assoc a name from to isMarked) =
  printArrow a
  ++ [TailLabel (mult from), HeadLabel (mult to)]
  ++ concat [maybeToList marking | isMarked] ++ [toLabel name | printNames]
  where
    printArrow
      | printNavigations = arrowDirected
      | otherwise        = arrow

arrowDirected :: AssociationType -> [Attribute]
arrowDirected Association = [arrowTo vee, ArrowSize 0.4]
arrowDirected a           = arrow a

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

drawCdFromSyntax :: Bool -> Bool -> Maybe Attribute -> Syntax -> FilePath -> GraphvizOutput -> IO ()
drawCdFromSyntax printNavigations printNames marking syntax file format = do
  let (classes, associations) = syntax
  let classNames = map fst classes
  let theNodes = classNames
  let inhEdges = [( fromJust (elemIndex from theNodes)
                  , fromJust (elemIndex to theNodes)
                  , Inheritance)
                 | (from, tos) <- classes, to <- tos]
  let classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
        where
          subs seen name
            | name `elem` seen = []
            | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((name `elem`) . snd) classes)
  let assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  let assocEdges = map (\(a,n,m1,from,to,m2) -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Assoc a n m1 m2 (shouldBeMarked from to classesWithSubclasses assocsBothWays))) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams {
                   fmtNode = \(_,l) -> [toLabel l,
                                        shape BoxShape, Margin $ DVal 0.04, Width 0, Height 0, FontSize 11],
                   fmtEdge = \(_,_,l) -> FontSize 11 : connectionArrow printNavigations printNames marking l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

drawOdFromInstance :: Map String DirType -> Bool -> String -> FilePath -> GraphvizOutput -> IO ()
drawOdFromInstance navigations printNames input file format = do
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
  let theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
  let theEdges = map ((\[from,v,to] -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), takeWhile (/= '$') v)) . splitOn "->") $
                 filter (not . null) (splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine)))))
  let numberedNodes = zip [0..] theNodes
  let graph = mkGraph numberedNodes theEdges :: Gr String String
  objectNames <-
    map (\(i, l) -> (i, let [n,z] = splitOn "$" l in firstLower n ++ (if z == "0" then "" else z) ++ " "))
    <$> drop (length theNodes `div` 3)
    <$> shuffleM numberedNodes
  let dotGraph = graphToDot (nonClusteredParams {
                   fmtNode = \(i,l) -> [underlinedLabel (fromMaybe "" (lookup i objectNames) ++ ": " ++ takeWhile (/= '$') l),
                                        shape BoxShape, Margin $ DVal $ 0.04, Width 0, Height 0, FontSize 12],
                   fmtEdge = \(_,_,l) -> arrowHeads l ++ [ArrowSize 0.4, FontSize 12] ++ [toLabel l | printNames] }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphvizCommand undirCommand dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output
  where
    arrowHeads l = case M.lookup l navigations of
      Nothing  -> [edgeEnds NoDir]
      Just dir -> [edgeEnds dir, arrowFrom vee, arrowTo vee]
