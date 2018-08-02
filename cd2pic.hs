module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)

import Data.List
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.Environment (getArgs)
import System.FilePath (dropExtension)

data Connection = Inheritance | Assoc (Int, Maybe Int) (Int, Maybe Int)

connectionArrow :: Connection -> [Attribute]
connectionArrow Inheritance = [arrowTo emptyArr]
connectionArrow (Assoc from to) = [ArrowHead noArrow, TailLabel (mult from), HeadLabel (mult to)]
  where mult (l, Nothing) = toLabelValue (show l ++ "..*")
        mult (l, Just u) | l == u = toLabelValue l
                         | otherwise = toLabelValue (show l ++ ".." ++ show u)

draw :: FilePath -> GraphvizOutput -> String -> IO ()
draw file format input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
  let (classes, associations) = syntax
  let theNodes = map fst classes
  let inhEdges = mapMaybe (\(from,mto) -> fmap (\to -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Inheritance)) mto) classes
  let assocEdges = map (\(_,m1,from,to,m2) -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Assoc m1 m2)) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [toLabel l, shape BoxShape], fmtEdge = \(_,_,l) -> connectionArrow l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= draw "output" Pdf
   [file] -> readFile file >>= draw file Pdf
   [file, format] -> readFile file >>= draw file (read (firstUpper format))
   _ -> error "zu viele Parameter"
