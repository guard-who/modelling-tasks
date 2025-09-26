module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.PlantUml.IO         ()
import Capabilities.WriteFile.IO        ()
import Modelling.ActivityDiagram.SelectAS (
  defaultSelectASConfig,
  selectAS,
  selectASTask,
  selectASSyntax,
  selectASEvaluation
  )
import Control.OutputCapable.Blocks     (Language (English))
import System.Environment               (getArgs)

import Common                           (withLang)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [path, s, seed] -> do
      putStrLn $ "Segment: " ++ s
      putStrLn $ "Seed: " ++ seed
      task <- selectAS defaultSelectASConfig (read s) (read seed)
      print task
      selectASTask path task `withLang` English
      sub <- read <$> getLine
      selectASSyntax task sub `withLang` English
      _ <- selectASEvaluation task sub `withLang` English
      return ()
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
