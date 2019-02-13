module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

run :: FilePath -> GraphvizOutput -> String -> IO ()
run file format input = do
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
   [] -> getContents >>= run "output" Pdf
   [file] -> readFile file >>= run file Pdf
   [file, format] -> readFile file >>= run file (read (firstUpper format))
   _ -> error "zu viele Parameter"
