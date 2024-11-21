-- |

module Modelling.CdOd.MatchCdOd.Config where

import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  )
import Modelling.CdOd.Types (
  CdMutation (..),
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  RelationshipMutation (..),
  )

import Data.Ratio                       ((%))

{-|
points: 0.15
generation time: 2:00min
CPU usage: 150%
-}
task2023_14 :: MatchCdOdConfig
task2023_14 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
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
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }

{-|
points: 0.15
generation time: 1:10min
CPU usage: 150%
-}
task2023_15 :: MatchCdOdConfig
task2023_15 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
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
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just False,
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
  withNonTrivialInheritance = Just True
  }

{-|
points: 0.15
generation time: 16:54min
CPU usage: 134%
-}
task2024_17 :: MatchCdOdConfig
task2024_17 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeLimit
    ],
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
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }

{-|
points: 0.15
generation time: 13:14min
CPU usage: 130%
-}
task2024_18 :: MatchCdOdConfig
task2024_18 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship Flip
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (5, Just 6),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (5, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }

{-|
points: 0.15
generation time: 5:14min
CPU usage: 127%
-}
task2024_19 :: MatchCdOdConfig
task2024_19 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (7, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 8)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Nothing
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }

{-|
points: 0.15
generation time: 2:44min
CPU usage: 133%
-}
task2024_20 :: MatchCdOdConfig
task2024_20 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (6, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (5, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 7 % 8,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Nothing
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True
  }
