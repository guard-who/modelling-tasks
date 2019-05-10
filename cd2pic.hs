module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

run :: Maybe Style -> String -> FilePath -> GraphvizOutput -> IO ()
run how input = do
  let tokens = lexer input
  let syntax = parser tokens
  drawCdFromSyntax False True how syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run Nothing contents "output" Pdf
   [file] -> readFile file >>= \contents -> run Nothing contents file Pdf
   [file, format] -> readFile file >>= \contents -> run Nothing contents file (read (firstUpper format))
   [file, format, x] -> readFile file >>= \contents -> run (Just $ specialStyle !! read x) contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"

specialStyle :: [Style]
specialStyle = [dashed, dotted, bold]
