module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Capabilities.PlantUml.IO         ()
import Modelling.ActivityDiagram.SelectPetri (
  defaultSelectPetriConfig,
  selectPetri,
  selectPetriTask,
  selectPetriSyntax,
  selectPetriEvaluation
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
      task <- selectPetri defaultSelectPetriConfig (read s) (read seed)
      print task
      selectPetriTask path task `withLang` English
      sub <- read <$> getLine
      selectPetriSyntax task sub `withLang` English
      _ <- selectPetriEvaluation task sub `withLang` English
      return ()
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
