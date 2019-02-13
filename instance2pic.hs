module Main (main) where

import Util
import Output

import Data.GraphViz

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> drawOdFromInstance contents "output" Pdf
   [file] -> readFile file >>= \contents -> drawOdFromInstance contents file Pdf
   [file, format] -> readFile file >>= \contents -> drawOdFromInstance contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"
