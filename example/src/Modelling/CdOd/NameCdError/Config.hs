-- |

module Modelling.CdOd.NameCdError.Config where

import Modelling.CdOd.NameCdError (
  NameCdErrorConfig (..),
  NumberOfReasons (..),
  Reason (..),
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  ArticlePreference (..),
  ClassConfig (..),
  CdConstraints (..),
  CdDrawSettings (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  Property (..),
  )

import Data.Ratio                       ((%))

{-|
points: 0.15
generation time: 0:20min
CPU usage: 200%
-}
task09 :: NameCdErrorConfig
task09 = NameCdErrorConfig {
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
    printNames = True,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True
  }

{-|
points: 0.15
generation time: 0:20min
CPU usage: 200%
-}
task10 :: NameCdErrorConfig
task10 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
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
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (8, Just 8)
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
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True
  }
