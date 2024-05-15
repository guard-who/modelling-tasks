-- |

module Modelling.CdOd.MatchCdOd.Config where

import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  )
import Modelling.CdOd.Types (
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  )

task14 :: MatchCdOdConfig
task14 = MatchCdOdConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 1),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }

task15 :: MatchCdOdConfig
task15 = MatchCdOdConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 4000,
  objectConfig = ObjectConfig {
    linkLimits = (2, Just 6),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (3, 6)
    },
  objectProperties = ObjectProperties {
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }
