module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

draw :: FilePath -> GraphvizOutput -> String -> IO ()
draw file format input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
  drawCdFromSyntax file format syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= draw "output" Pdf
   [file] -> readFile file >>= draw file Pdf
   [file, format] -> readFile file >>= draw file (read (firstUpper format))
   _ -> error "zu viele Parameter"
