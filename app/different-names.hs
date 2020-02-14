module Main (main) where

import Alloy.CdOd.MatchCdOd             (defaultMatchCdOdConfig)
import Alloy.CdOd.NaiveTasks            (differentNames)
import EvaluateArgs                     (evaluateArgs)

import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  differentNames defaultMatchCdOdConfig "output" s seed >>= print
