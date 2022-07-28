module Main (main) where

import Common                           ()

import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNamesTask)
import Modelling.CdOd.Generate.DifferentNames (differentNames)
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (LangM'(withLang), Language (English))
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
