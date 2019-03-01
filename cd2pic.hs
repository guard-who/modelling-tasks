module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

run :: String -> FilePath -> GraphvizOutput -> IO ()
run input = do
  let tokens = lexer input
  let syntax = parser tokens
  drawCdFromSyntax False True syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run contents "output" Pdf
   [file] -> readFile file >>= \contents -> run contents file Pdf
   [file, format] -> readFile file >>= \contents -> run contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"
