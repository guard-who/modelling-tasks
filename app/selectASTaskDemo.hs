module Main (main) where

import Modelling.ActivityDiagram.SelectAS (
  defaultSelectASConfig,
  selectAS,
  selectASTask,
  selectASSyntax,
  selectASEvaluation
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
      task <- selectAS defaultSelectASConfig (read s) (read seed)
      print task
      selectASTask path task `withLang` English
      sub <- read <$> getLine
      selectASSyntax task sub `withLang` English
      _ <- selectASEvaluation task sub `withLang` English
      return ()
