module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

run :: Attribute -> String -> FilePath -> GraphvizOutput -> IO ()
run howToMark input = do
  let tokens = lexer input
  let syntax = parser tokens
  drawCdFromSyntax False (Just howToMark) syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run redColor contents "output" Pdf
   [file] -> readFile file >>= \contents -> run redColor contents file Pdf
   [file, format] -> readFile file >>= \contents -> run redColor contents file (read (firstUpper format))
   [file, format, x] -> readFile file >>= \contents -> run (specialStyle !! read x) contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"

specialStyle :: [Attribute]
specialStyle = map style [dashed, dotted, bold]
