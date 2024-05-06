module Main (main) where

import Common                           (withLang)

import Capabilities.Alloy.IO            ()
import Capabilities.Diagrams.IO         ()
import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNamesTask)
import Modelling.CdOd.Generate.DifferentNames (differentNames)
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (Language (English))
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  i <- differentNames defaultDifferentNamesConfig s seed
  print i
  differentNamesTask "output" i `withLang` English
