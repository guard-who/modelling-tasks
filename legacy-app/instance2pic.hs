module Main (main) where

import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.Output

import Control.Monad (void)
import Data.GraphViz
import Data.Map      (empty)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> getContents >>= drawOd "output" Pdf
   [file] -> readFile file >>= drawOd file Pdf
   [file, format] -> readFile file >>= drawOd file (read (firstUpper format))
   _ -> error "zu viele Parameter"

drawOd :: FilePath -> GraphvizOutput -> String -> IO ()
drawOd file format contents = do
  output <- drawOdFromRawInstance contents empty False file format
  putStrLn $ "Output written to " ++ output
