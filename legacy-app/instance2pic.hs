module Main (main) where

import Modelling.CdOd.Output

import Control.Monad (void)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Data.Char                        (toUpper)
import Data.Map      (empty)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> getContents >>= drawOd "output"
   [file] -> readFile file >>= drawOd file
   [file, format]
     | fmap toUpper format == "SVG" -> readFile file >>= drawOd file
   _ -> error "zu viele Parameter"

drawOd :: FilePath -> String -> IO ()
drawOd file contents = do
  output <- flip evalRandT (mkStdGen 0) $
    drawOdFromRawInstance contents empty False file
  putStrLn $ "Output written to " ++ output
