module Alloy.CdOd.Output (
  drawCdFromSyntax,
  drawOdFromInstance,
  drawOdFromRawInstance,
  ) where

import qualified Data.Map               as M (lookup)
import qualified Data.Set               as S (toList)

import Alloy.CdOd.Auxiliary.Util        (emptyArr, firstLower, underlinedLabel)
import Alloy.CdOd.Types
  (AssociationType(..), Connection(..), Syntax)
import Alloy.CdOd.Edges                 (shouldBeMarked)

import Data.Graph.Inductive             (Gr, mkGraph)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List                        (elemIndex, isPrefixOf, stripPrefix)
import Data.List.Split                  (splitOn)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, fromMaybe, maybeToList)
import Language.Alloy.Call
  (AlloyInstance, getSingle, getTriple, lookupSig, objectName, scoped)
import System.FilePath                  (dropExtension)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Random.Shuffle            (shuffleM)

connectionArrow :: Bool -> Bool -> Maybe Attribute -> Connection -> [Attribute]
connectionArrow _ _ _ Inheritance =
  [arrowTo emptyArr]
connectionArrow _ printNames marking (Assoc Composition name from to isMarked) =
  arrow Composition ++ [HeadLabel (mult to)]
  ++ concat [maybeToList marking | isMarked] ++ [toLabel name | printNames]
  ++ case from of
       (1, Just 1) -> []
       (0, Just 1) -> [TailLabel (mult from)]
       (0, Nothing)-> [TailLabel $ toLabelValue "0..*"]
       (_, _)      -> unsafePerformIO $ do
         putStrLn "invalid composition multiplicity"
         return [TailLabel (mult from)]
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
mult (-1, Just u) = toLabelValue $ "*.." ++ show u
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

drawOdFromInstance :: AlloyInstance -> Map String DirType -> Bool -> FilePath -> GraphvizOutput -> IO ()
drawOdFromInstance i =
  let g = either error id $ do
        os    <- lookupSig (scoped "this" "Obj") i
        objs  <- fmap objectName . S.toList <$> getSingle "" os
        links <- fmap (linkOf objs) . S.toList <$> getTriple "get" os
        return (objs, links)
  in uncurry drawOdFromNodesAndEdges g
  where
    nameOf   = takeWhile (/= '$') . objectName
    linkOf objs (x, l, y) =
      let indexOf z = fromJust $ elemIndex (objectName z) objs
      in (indexOf x, indexOf y, nameOf l)

drawOdFromRawInstance :: String -> Map String DirType -> Bool -> FilePath -> GraphvizOutput -> IO ()
drawOdFromRawInstance input =
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
      theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
      theEdges = map ((\[from,v,to] -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), takeWhile (/= '$') v)) . splitOn "->") $
                 filter (not . null) (splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine)))))
  in drawOdFromNodesAndEdges theNodes theEdges

drawOdFromNodesAndEdges :: [String] -> [(Int, Int, String)] -> Map String DirType -> Bool -> FilePath -> GraphvizOutput -> IO ()
drawOdFromNodesAndEdges theNodes theEdges navigations printNames file format = do
  let numberedNodes = zip [0..] theNodes
  let graph = mkGraph numberedNodes theEdges :: Gr String String
  objectNames <-
    map (\(i, l) -> (i, let [n,z] = splitOn "$" l in firstLower n ++ (if z == "0" then "" else z) ++ " "))
    . drop (length theNodes `div` 3)
    <$> shuffleM numberedNodes
  let dotGraph = graphToDot (nonClusteredParams {
                   fmtNode = \(i,l) -> [underlinedLabel (fromMaybe "" (lookup i objectNames) ++ ": " ++ takeWhile (/= '$') l),
                                        shape BoxShape, Margin $ DVal 0.04, Width 0, Height 0, FontSize 12],
                   fmtEdge = \(_,_,l) -> arrowHeads l ++ [ArrowSize 0.4, FontSize 12] ++ [toLabel l | printNames] }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphvizCommand undirCommand dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output
  where
    arrowHeads l = case M.lookup l navigations of
      Nothing  -> [edgeEnds NoDir]
      Just dir -> [edgeEnds dir, arrowFrom vee, arrowTo vee]
