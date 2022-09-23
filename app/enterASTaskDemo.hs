module Main (main) where

import Modelling.ActivityDiagram.EnterAS (
  defaultEnterASConfig,
  enterAS,
  enterASTask,
  enterASSyntax,
  enterASEvaluation
  )
import Control.Monad.Output             (LangM' (withLang), Language (English))
import System.Environment               (getArgs)

import Common ()

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [path, s, seed] -> do
      putStrLn $ "Segment: " ++ s
      putStrLn $ "Seed: " ++ seed
      task <- enterAS defaultEnterASConfig (read s) (read seed)
      print task
      enterASTask path task `withLang` English
      sub <- read <$> getLine
      enterASSyntax task sub `withLang` English
      points <- enterASEvaluation task sub `withLang` English
      print points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
