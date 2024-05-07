module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.PlantUml.IO         ()
import Modelling.ActivityDiagram.MatchAd (
  defaultMatchAdConfig,
  matchAd,
  matchAdTask,
  matchAdSyntax,
  matchAdEvaluation
  )
import Control.Monad.Output             (Language (English))
import System.Environment               (getArgs)

import Common                           (withLang)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [path, s, seed] -> do
      putStrLn $ "Segment: " ++ s
      putStrLn $ "Seed: " ++ seed
      task <- matchAd defaultMatchAdConfig (read s) (read seed)
      print task
      matchAdTask path task `withLang` English
      sub <- read <$> getLine
      matchAdSyntax task sub `withLang` English
      points <- matchAdEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
