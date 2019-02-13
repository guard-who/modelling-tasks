module Main (main) where

import Util
import Lexer (lexer)
import Parser (parser)
import Output

import Data.GraphViz

import System.Environment (getArgs)

run :: String -> FilePath -> GraphvizOutput -> IO ()
run input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
  drawCdFromSyntax syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run contents "output" Pdf
   [file] -> readFile file >>= \contents -> run contents file Pdf
   [file, format] -> readFile file >>= \contents -> run contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"
