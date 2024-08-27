module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.PlantUml.IO         ()
import Modelling.ActivityDiagram.FindSupportPetriNode (
  defaultFindSupportPetriNodeConfig,
  findSupportPetriNode,
  findSupportPetriNodeTask,
  findSupportPetriNodeEvaluation
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
      task <- findSupportPetriNode
        defaultFindSupportPetriNodeConfig
        (read s)
        (read seed)
      print task
      findSupportPetriNodeTask path task `withLang` English
      sub <- read <$> getLine
      points <- findSupportPetriNodeEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
