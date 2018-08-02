module Main (main) where

import Util

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz

import System.Environment (getArgs)
import System.FilePath (dropExtension)

draw :: FilePath -> GraphvizOutput -> String -> IO ()
draw file format input = do
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
  let theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
  let theEdges = map ((\[from,_,to] -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), ())) . splitOn "->") $
                 splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine))))
  let graph = undir (mkGraph (zip [0..] theNodes) theEdges) :: Gr String ()
  let dotGraph = setDirectedness graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [underlinedLabel (firstLower l ++ " : " ++ takeWhile (/= '$') l), shape BoxShape] }) graph
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
