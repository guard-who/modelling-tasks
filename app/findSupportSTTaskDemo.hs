module Main (main) where

import Modelling.ActivityDiagram.FindSupportST (
  defaultFindSupportSTConfig,
  findSupportST,
  findSupportSTTask,
  findSupportSTEvaluation
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
      task <- findSupportST defaultFindSupportSTConfig (read s) (read seed)
      print task
      findSupportSTTask path task `withLang` English
      sub <- read <$> getLine
      points <- findSupportSTEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
