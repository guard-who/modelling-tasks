module Main (main) where

import Common                           ()
import Modelling.CdOd.Types             (ClassConfig (..))
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  matchCdOd,
  matchCdOdTask,
  )
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (LangM' (withLang), Language (English))
import System.Environment               (getArgs)

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
          maxLinksPerObject = Nothing,
          maxObjects       = 4,
          maxInstances     = Nothing,
          minLinks         = Nothing,
          minLinksPerObject = Nothing,
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
