-- |

module Modelling.CdOd.SelectValidCd.Config where

import Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  ArticlePreference (..),
  ClassConfig (..),
  CdConstraints (..),
  CdDrawSettings (..),
  CdMutation (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  RelationshipMutation (..),
  )

import Data.Ratio                       ((%))

{-|
points: 0.15
average generation time per instance: 24:00h
CPU usage: 400%
-}
task2023_05 :: SelectValidCdConfig
task2023_05 = SelectValidCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Just True
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (7, Just 8)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }

{-|
points: 0.15
average generation time per instance: 1:30h
CPU usage: 400%
-}
task2023_06 :: SelectValidCdConfig
task2023_06 = SelectValidCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = True,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 3),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 8)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = False,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }

{-|
points: 0.15
average generation time per instance: 24:00h
CPU usage: 400%
-}
task2024_06 :: SelectValidCdConfig
task2024_06 = SelectValidCdConfig {
  allowedCdMutations = [
    AddRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Just True
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (7, Just 8)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }

{-|
points: 0.15
average generation time per instance: 3:18min
CPU usage: 278%
-}
task2024_07 :: SelectValidCdConfig
task2024_07 = SelectValidCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = True,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (0, Just 1),
    relationshipLimits = (6, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }

{-|
points: 0.15
average generation time per instance: 1:30h
CPU usage: 400%
-}
task2024_08 :: SelectValidCdConfig
task2024_08 = SelectValidCdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = True
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }

{-|
points: 0.8
average generation time per instance: ?:??h
CPU usage: ???%
-}
task2024_51 :: SelectValidCdConfig
task2024_51 = SelectValidCdConfig {
  allowedCdMutations = [
    AddRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Just True
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (6, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = False
    },
  maxInstances = Just 2000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = False,
  timeout = Nothing
  }

{-|
points: 0.08
average generation time per instance: ?:??h
CPU usage: ???%
-}
task2024_52 :: SelectValidCdConfig
task2024_52 = SelectValidCdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = True
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  shuffleEachCd = False,
  timeout = Nothing
  }
