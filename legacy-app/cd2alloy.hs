module Main (main) where

import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.CD2Alloy.Transform (transform)

import Control.Monad
import Data.Time.LocalTime
import System.Environment (getArgs)

run :: String -> Maybe FilePath -> Bool -> String -> IO ()
run input output template index = do
  let tokens = lexer input
  let syntax = parser tokens
  time <- getZonedTime
  let (part1, part2, part3, part4, part5) = transform syntax index (show time)
  case output of
    Just file -> do
      when template $ let out = file ++ ".part1" in writeFile out part1 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part2" in writeFile out part2 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part3" in writeFile out part3 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part4" in writeFile out part4 >> putStrLn ("Some output written to " ++ out)
      when template $ let out = file ++ ".part5" in writeFile out part5 >> putStrLn ("Some output written to " ++ out)
    Nothing -> putStrLn $ (if template then part1 else "") ++ part2 ++ part3 ++ part4 ++ (if template then part5 else "")

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run contents Nothing False ""
   [input] -> readFile input >>= \contents -> run contents Nothing False ""
   [input, output] -> readFile input >>= \contents -> run contents (Just output) True ""
   [input, output, index] -> readFile input >>= \contents -> run contents (Just output) True (show (read index :: Int))
   _ -> error "zu viele Parameter"
