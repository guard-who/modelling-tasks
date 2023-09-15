{-# OPTIONS_GHC -Wwarn=deprecations #-}
module Main (main) where

import Common                           (withLang)
import Modelling.CdOd.Types (
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  )
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  matchCdOd,
  matchCdOdTask,
  )
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (Language (English))
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  let config = MatchCdOdConfig {
          classConfig = ClassConfig {
              classLimits        = (4, 4),
              aggregationLimits  = (0, Just 2),
              associationLimits  = (0, Just 2),
              compositionLimits  = (0, Just 1),
              inheritanceLimits  = (1, Just 2),
              relationshipLimits = (4, Just 6)
            },
          objectConfig = ObjectConfig {
            linkLimits           = (0, Nothing),
            linksPerObjectLimits = (0, Nothing),
            objectLimits         = (2, 4)
            },
          maxInstances     = Nothing,
          objectProperties = ObjectProperties {
            completelyInhabited = Nothing,
            hasLimitedIsolatedObjects = True,
            hasSelfLoops = Nothing,
            usesEveryRelationshipName = Nothing
            },
          printSolution    = False,
          searchSpace      = 10,
          timeout          = Nothing
        }
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  task <- matchCdOd config s seed
  print task
  matchCdOdTask "" task `withLang` English
