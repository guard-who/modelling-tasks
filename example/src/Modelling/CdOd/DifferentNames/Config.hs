-- |

module Modelling.CdOd.DifferentNames.Config where

import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  )
import Modelling.CdOd.Types (
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  )

{-|
points: 0.15
generation time: 0:27min
CPU usage: 350%
-}
task12 :: DifferentNamesConfig
task12 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (6, Just 6)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 4000,
  objectConfig = ObjectConfig {
    linkLimits = (10, Just 10),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (8, 8)
    },
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
    },
  onlyAnonymousObjects = True,
  printSolution = True,
  timeout = Nothing
  }

{-|
points: 0.15
generation time: 1:40min
CPU usage: 350%
-}
task13 :: DifferentNamesConfig
task13 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (11, Just 11),
    linksPerObjectLimits = (1, Just 6),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
    },
  onlyAnonymousObjects = False,
  printSolution = True,
  timeout = Nothing
  }

{-|
points: 0.25
generation time: 3:00min
CPU usage: 150%
-}
task25 :: DifferentNamesConfig
task25 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (3, Just 3),
    relationshipLimits = (9, Just 9)
    },
  withNonTrivialInheritance = Just True,
    maxInstances = Just 100,
    objectConfig = ObjectConfig {
    linkLimits = (14, Just 16),
    linksPerObjectLimits = (2, Just 6),
    objectLimits = (8, 10)
    },
  objectProperties = ObjectProperties {
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
    },
  onlyAnonymousObjects = True,
  printSolution = True,
  timeout = Nothing
  }
