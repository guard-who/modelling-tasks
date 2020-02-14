module Main where

import Alloy.CdOd.NaiveTasks            (RepairCdInstance (..), phraseChange, repairCd, selectValidCd)
import Alloy.CdOd.Types                 (ClassConfig (..))
import EvaluateArgs                     (evaluateArgs)

import Control.Arrow                    (second)
import System.Environment               (getArgs)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (4, 4),
          aggregations = (0, Just 2),
          associations = (0, Just 2),
          compositions = (0, Just 3),
          inheritances = (1, Just 3)
        }
  repair:args <- getArgs
  (s, seed)   <- evaluateArgs args
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  if read repair
    then do
    task <- repairCd config "repair" s seed
    print $ second phraseChange <$> changes task
    print task
    else selectValidCd config "select" s seed >>= print
