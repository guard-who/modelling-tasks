module Output where

import Util
import Types (AssociationType(..), Connection(..), Syntax)

import Data.List
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.FilePath (dropExtension)

connectionArrow :: Connection -> [Attribute]
connectionArrow Inheritance = [arrowTo emptyArr]
connectionArrow (Assoc Composition from to isRed) =
  case from of
    (1, Just 1) -> arrow Composition ++ [HeadLabel (mult to)] ++ [redColor | isRed]
    (0, Just 1) -> arrow Composition ++ [TailLabel (mult from), HeadLabel (mult to)] ++ [redColor | isRed]
connectionArrow (Assoc a from to isRed) = arrow a ++ [TailLabel (mult from), HeadLabel (mult to)] ++ [redColor | isRed]

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

drawCdFromSyntax :: FilePath -> GraphvizOutput -> Syntax -> IO ()
drawCdFromSyntax file format syntax = do
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
  let assocEdges = map (\(a,_,m1,from,to,m2) -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Assoc a m1 m2 (shouldBeRed from to classesWithSubclasses assocsBothWays))) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [toLabel l, shape BoxShape], fmtEdge = \(_,_,l) -> connectionArrow l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

shouldBeRed :: String -> String -> [(String, [String])] -> [(String, String)] -> Bool
shouldBeRed a b classesWithSubclasses = any (\(a',b') ->
                                               (a /= a' || b /= b')
                                               && let { one = a' `isSubOf` a; two = b' `isSubOf` b }
                                                  in (one && (two || b `isSubOf` b') || two && (one || a `isSubOf` a'))
                                            )
  where x `isSubOf` y = x `elem` fromJust (lookup y classesWithSubclasses)
