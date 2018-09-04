module Main (main) where

import Util
import Types
import Lexer (lexer)
import Parser (parser)

import Data.List
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.Environment (getArgs)
import System.FilePath (dropExtension)

data Connection = Inheritance | Assoc AssociationType (Int, Maybe Int) (Int, Maybe Int) Bool

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

draw :: FilePath -> GraphvizOutput -> String -> IO ()
draw file format input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
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

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= draw "output" Pdf
   [file] -> readFile file >>= draw file Pdf
   [file, format] -> readFile file >>= draw file (read (firstUpper format))
   _ -> error "zu viele Parameter"
