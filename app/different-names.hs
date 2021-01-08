module Main (main) where

import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNames)
import EvaluateArgs                     (evaluateArgs)

import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  differentNames defaultDifferentNamesConfig "output" s seed >>= print
