module Main (main) where

import Modelling.CdOd.Output

import Control.Monad (void)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Data.Char                        (toUpper)
import Data.GraphViz                    (DirType (NoDir))

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> getContents >>= drawOd "output"
   [file] -> readFile file >>= drawOd file
   [file, format]
     | fmap toUpper format == "SVG" -> readFile file >>= drawOd file
     | otherwise -> error $ "format " ++ format
         ++ "is not supported, only SVG is supported"
   _ -> error "zu viele Parameter"

drawOd :: FilePath -> String -> IO ()
drawOd file contents = do
  output <- flip evalRandT (mkStdGen 0) $
    drawOdFromRawInstance contents NoDir False file
  putStrLn $ "Output written to " ++ output
