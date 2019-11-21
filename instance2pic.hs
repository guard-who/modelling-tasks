module Main (main) where

import Auxiliary.Util
import Output

import Data.GraphViz
import Data.Map      (empty)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> drawOdFromInstance empty False contents "output" Pdf
   [file] -> readFile file >>= \contents -> drawOdFromInstance empty False contents file Pdf
   [file, format] -> readFile file >>= \contents -> drawOdFromInstance empty False contents file (read (firstUpper format))
   _ -> error "zu viele Parameter"
