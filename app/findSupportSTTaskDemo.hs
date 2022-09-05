module Main (main) where

import Modelling.ActivityDiagram.FindSupportST (
  FindSupportSTSolution(..),
  defaultFindSupportSTConfig,
  findSupportST,
  findSupportSTTask,
  findSupportSTEvaluation
  )
import Control.Monad.Output             (LangM' (withLang), Language (English))
import System.Environment               (getArgs)

import Common ()

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    path:s:seed:xs' -> do
      putStrLn $ "Segment: " ++ s
      putStrLn $ "Seed: " ++ seed
      task <- findSupportST defaultFindSupportSTConfig (read s) (read seed)
      print task
      findSupportSTTask path task `withLang` English
      putStrLn $ "Enter number of nodes in net:"
      petriNodes <- read <$> getLine
      putStrLn $ "Enter number of support places in net:"
      supportPlaces <- read <$> getLine
      putStrLn $ "Enter number of support transitions in net:"
      supportTransitions <- read <$> getLine
      let sub = FindSupportSTSolution {
        numberOfPetriNodes = petriNodes,
        numberOfSupportPlaces = supportPlaces,
        numberOfSupportTransitions = supportTransitions
      }
      points <- findSupportSTEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"