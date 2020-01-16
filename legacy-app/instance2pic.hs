module Main (main) where

import Alloy.CdOd.Auxiliary.Util
import Alloy.CdOd.Output

import Data.GraphViz
import Data.Map      (empty)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> drawOdFromRawInstance contents empty False "output" Pdf
   [file] -> readFile file >>= \contents -> drawOdFromRawInstance contents empty False file Pdf
   [file, format] -> readFile file >>= \contents -> drawOdFromRawInstance contents empty False file (read (firstUpper format))
   _ -> error "zu viele Parameter"
