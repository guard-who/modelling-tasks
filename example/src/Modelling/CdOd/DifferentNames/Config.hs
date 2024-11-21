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

import Data.Ratio                       ((%))

{-|
points: 0.15
generation time: 0:27min
CPU usage: 350%
-}
task2023_12 :: DifferentNamesConfig
task2023_12 = DifferentNamesConfig {
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
    anonymousObjectProportion = 1 % 1,
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
  printSolution = True,
  timeout = Nothing,
  withObviousMapping = Nothing
  }

{-|
points: 0.15
generation time: 1:40min
CPU usage: 350%
-}
task2023_13 :: DifferentNamesConfig
task2023_13 = DifferentNamesConfig {
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
    anonymousObjectProportion = 0 % 1,
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
  printSolution = True,
  timeout = Nothing,
  withObviousMapping = Nothing
  }

{-|
points: 0.25
generation time: 3:00min
CPU usage: 150%
-}
task2023_25 :: DifferentNamesConfig
task2023_25 = DifferentNamesConfig {
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
    anonymousObjectProportion = 1 % 1,
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
  printSolution = True,
  timeout = Nothing,
  withObviousMapping = Nothing
  }

{-|
points: 0.15
generation time: 1:10min
CPU usage: 355%
-}
task2024_15 :: DifferentNamesConfig
task2024_15 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (6, Just 6)
    },
  withNonTrivialInheritance = Just False,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (9, Just 9),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (7, 7)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withObviousMapping = Nothing
  }

{-|
points: 0.15
generation time: 1:17min
CPU usage: 346%
-}
task2024_16 :: DifferentNamesConfig
task2024_16 = DifferentNamesConfig {
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
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just True,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withObviousMapping = Nothing
  }
