module Main (main) where

import Modelling.ActivityDiagram.MatchAD (
  defaultMatchADConfig,
  matchAD,
  matchADTask,
  matchADSyntax,
  matchADEvaluation
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
      task <- matchAD defaultMatchADConfig (read s) (read seed)
      print task
      matchADTask path task `withLang` English
      sub <- read <$> getLine
      matchADSyntax task sub `withLang` English
      points <- matchADEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"