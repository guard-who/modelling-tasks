module Main (main) where

import Util
import Output

import Data.GraphViz

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= drawOdFromInstance "output" Pdf
   [file] -> readFile file >>= drawOdFromInstance file Pdf
   [file, format] -> readFile file >>= drawOdFromInstance file (read (firstUpper format))
   _ -> error "zu viele Parameter"
