-- |

module Modelling.CdOd.SelectValidCd.Config where

import Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  ArticlePreference (..),
  ClassConfig (..),
  CdDrawSettings (..),
  CdMutation (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  RelationshipMutation (..),
  )

import Data.Ratio                       ((%))

{-|
points: 0.15
generation time: 24:00h
CPU usage: 400%
-}
task05 :: SelectValidCdConfig
task05 = SelectValidCdConfig {
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
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
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
generation time: 1:30h
CPU usage: 400%
-}
task06 :: SelectValidCdConfig
task06 = SelectValidCdConfig {
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
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
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
