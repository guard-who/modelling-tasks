-- |

module Modelling.CdOd.RepairCd.Config where

import Modelling.CdOd.RepairCd (
  RepairCdConfig (..),
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
generation time: 5:00h
CPU usage: 600%
-}
task07 :: RepairCdConfig
task07 = RepairCdConfig {
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
    wrongAssociationLimits = False,
    wrongCompositionLimits = True
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
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 1000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = True
  }

{-|
points: 0.15
attention: invalid configuration! (increase maxInstances!)
-}
task08 :: RepairCdConfig
task08 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
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
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
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
  maxInstances = Just 1000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = False
  }
