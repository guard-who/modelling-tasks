module Main (main) where

import Capabilities.Alloy.IO            ()
import Common                           (withLang)

import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNames, differentNamesTask)
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (Language (English))
import Control.Monad.Trans.Except       (runExceptT)
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  i <- either error id
     <$> runExceptT (differentNames defaultDifferentNamesConfig s seed)
  print i
  differentNamesTask "output" i `withLang` English
