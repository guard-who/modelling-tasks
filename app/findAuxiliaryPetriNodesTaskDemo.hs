module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.PlantUml.IO         ()
import Capabilities.WriteFile.IO         ()
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  defaultFindAuxiliaryPetriNodesConfig,
  findAuxiliaryPetriNodes,
  findAuxiliaryPetriNodesTask,
  findAuxiliaryPetriNodesEvaluation
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
      task <- findAuxiliaryPetriNodes
        defaultFindAuxiliaryPetriNodesConfig
        (read s)
        (read seed)
      print task
      findAuxiliaryPetriNodesTask path task `withLang` English
      sub <- read <$> getLine
      points <- findAuxiliaryPetriNodesEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
