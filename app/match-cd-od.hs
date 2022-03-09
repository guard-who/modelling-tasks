module Main (main) where

import Common                           ()
import Modelling.CdOd.Types             (ClassConfig (..))
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  matchCdOd,
  matchCdOdTask,
  )
import EvaluateArgs                     (evaluateArgs)

import System.Environment               (getArgs)
import Modelling.Auxiliary.Output       (LangM' (withLang), Language (English))

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  let config = MatchCdOdConfig {
          classConfig = ClassConfig {
              classes      = (4, 4),
              aggregations = (0, Just 2),
              associations = (0, Just 2),
              compositions = (0, Just 1),
              inheritances = (1, Just 2)
            },
          maxLinks         = Nothing,
          maxObjects       = 4,
          maxInstances     = Nothing,
          minLinks         = Nothing,
          presenceOfLinkSelfLoops = Nothing,
          printSolution    = False,
          searchSpace      = 10,
          timeout          = Nothing
        }
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  task <- matchCdOd config s seed
  print task
  matchCdOdTask "" task `withLang` English
