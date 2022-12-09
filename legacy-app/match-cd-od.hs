{-# OPTIONS_GHC -Wwarn=deprecations #-}
module Main (main) where

import Common                           ()
import Modelling.CdOd.Types             (ClassConfig (..), ObjectConfig (..))
import Modelling.CdOd.Generate.MatchCdOd (
  matchCdOd,
  )
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
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
              inheritances = (1, Just 2),
              relationships = (4, Just 6)
            },
          objectConfig = ObjectConfig {
            links          = (0, Nothing),
            linksPerObject = (0, Nothing),
            objects        = (2, 4)
            },
          maxInstances     = Nothing,
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
