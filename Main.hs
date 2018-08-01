module Main (main) where

import Lexer (lexer)
import Parser (parser)
import Transform (transform)
import Data.Time.LocalTime

import System.Environment (getArgs)

run mfoname template index input = do
  -- putStrLn ("INPUT:\n\n" ++ show input ++ "\n")
  let tokens = lexer input
  -- putStrLn ("TOKENS:\n\n" ++ show tokens ++ "\n")
  let syntax = parser tokens
  -- putStrLn ("SYNTAX:\n\n" ++ show syntax ++ "\n")
  time <- getZonedTime
  let result = transform time template index syntax
  case mfoname of
    Just fo -> writeFile fo result
    Nothing -> putStrLn result

main = do
  args <- getArgs
  case args of
   [] -> getContents >>= run Nothing False ""
   [fname] -> readFile fname >>= run Nothing False ""
   [fname, foname] -> readFile fname >>= run (Just foname) True ""
   [fname, foname, index] -> readFile fname >>= run (Just foname) True (show (read index :: Int))
   _ -> error "zu viele Parameter"
