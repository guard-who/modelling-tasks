module Main where

import Modelling.CdOd.RepairCd (
  RepairCdConfig (..),
  RepairCdInstance (..),
  defaultRepairCdConfig,
  phraseChange,
  repairCd,
  )
import Modelling.CdOd.SelectValidCd
  (defaultSelectValidCdConfig, selectValidCd)
import EvaluateArgs                     (evaluateArgs)

import Control.Arrow                    (second)
import System.Environment               (getArgs)

main :: IO ()
main = do
  repair:args <- getArgs
  (s, seed)   <- evaluateArgs args
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  if read repair
    then do
    let name = useNames defaultRepairCdConfig
        dir  = printNavigations defaultRepairCdConfig
    task <- repairCd defaultRepairCdConfig "repair" s seed
    print $ second (phraseChange name dir) <$> changes task
    print task
    else selectValidCd defaultSelectValidCdConfig "select" s seed >>= print
