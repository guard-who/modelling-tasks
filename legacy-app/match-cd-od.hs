{-# OPTIONS_GHC -Wwarn=deprecations #-}
module Main (main) where

import Common                           (withLang)

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.Types (
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  defaultOmittedDefaultMultiplicities,
  )
import Modelling.CdOd.Generate.MatchCdOd (
  matchCdOd,
  )
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  matchCdOdTask,
  )
import EvaluateArgs                     (evaluateArgs)

import Control.OutputCapable.Blocks     (Language (English))
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
          omittedDefaultMultiplicities = defaultOmittedDefaultMultiplicities,
          printSolution    = False,
          timeout          = Nothing,
          withNonTrivialInheritance = Nothing
        }
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  task <- matchCdOd 10 config s seed
  print task
  matchCdOdTask "" task `withLang` English
