module Main (main) where

import Util
import Output

import Data.GraphViz

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> drawOdFromInstance False contents "output" Pdf
   [file] -> readFile file >>= \contents -> drawOdFromInstance False contents file Pdf
   [file, format] -> readFile file >>= \contents -> drawOdFromInstance False contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"
