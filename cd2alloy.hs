module Main (main) where

import Lexer (lexer)
import Parser (parser)
import Transform (transform)
import Data.Time.LocalTime

import System.Environment (getArgs)

run :: Maybe FilePath -> Bool -> String -> String -> IO ()
run output template index input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
  time <- getZonedTime
  let result = transform (show time) template index syntax
  case output of
    Just file -> writeFile file result >> putStrLn ("Output written to " ++ file)
    Nothing -> putStrLn result

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= run Nothing False ""
   [input] -> readFile input >>= run Nothing False ""
   [input, output] -> readFile input >>= run (Just output) True ""
   [input, output, index] -> readFile input >>= run (Just output) True (show (read index :: Int))
   _ -> error "zu viele Parameter"
