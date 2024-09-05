{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.GenerateSpec where

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.Edges (
  DiagramEdge,
  compositionCycles,
  doubleConnections,
  fromEdges,
  hasAssociationAtOneSuperclass,
  inheritanceCycles,
  multipleInheritances,
  selfEdges,
  toEdges,
  wrongLimits,
  )
import Modelling.CdOd.Generate (
  generateCds,
  instanceToAnyCd,
  )
import Modelling.CdOd.Types (
  AnyClassDiagram (..),
  ClassConfig (..),
  RelationshipProperties (..),
  anyThickEdge,
  defaultProperties,
  )

import Test.Hspec
import Test.QuickCheck                  (ioProperty)
import Control.Monad.Random             (evalRandTIO)

generateCd
  :: Maybe Bool
  -> ClassConfig
  -> RelationshipProperties
  -> Maybe Integer
  -> Maybe Int
  -> IO ([String], [DiagramEdge])
generateCd wi c p mis to = do
  i <- head <$> evalRandTIO (generateCds wi c p mis to)
  anyCd <- instanceToAnyCd i
  toEdges' anyCd
  where
    toEdges' x = (anyClassNames x,) <$> toEdges x

spec :: Spec
spec =
  describe "generate" $ do
    it "generates non trivial inheritance instances" $
      ioProperty $ do
        c <- generateCd (Just True) classConfig defaultProperties (Just 1000) Nothing
        return $ c `shouldSatisfy` uncurry hasAssociationAtOneSuperclass
    it "generates no non trivial inheritance instances" $
      ioProperty $ do
        c <- generateCd (Just False) classConfig defaultProperties (Just 1000) Nothing
        return $ c `shouldSatisfy` not . uncurry hasAssociationAtOneSuperclass
    generateProperty
      "wrongLimits (NonInheritance)"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongNonInheritances = 1 }
    generateProperty
      "wrongLimits (Composition)"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongCompositions = 1 }
    generateProperty
      "no wrongLimits"
      (const $ null . wrongLimits)
      defaultProperties {
        wrongNonInheritances = 0,
        wrongCompositions = 0
        }
    generateProperty
      "selfEdges (NonInheritance)"
      (const $ not . null . selfEdges)
      defaultProperties { selfRelationshipsAmount = 1 }
    generateProperty
      "selfEdges (Inheritance)"
      (const $ not . null . selfEdges)
      defaultProperties { selfInheritancesAmount = 1 }
    generateProperty
      "no selfEdges"
      (const $ null . selfEdges)
      defaultProperties {
        selfInheritancesAmount = 0,
        selfRelationshipsAmount = 0
        }
    generateProperty
      "doubleConnections (same direction)"
      (const $ not . null . doubleConnections)
      defaultProperties { hasDoubleRelationships = Just True }
    generateProperty
      "doubleConnections (reverse direction)"
      (const $ not . null . doubleConnections)
      defaultProperties { hasReverseRelationships = Just True }
    generateProperty
      "no doubleConnections"
      (const $ null . doubleConnections)
      defaultProperties {
        hasDoubleRelationships = Just False,
        hasReverseInheritances = False,
        hasReverseRelationships = Just False
        }
    generateProperty
      "multipleInheritances"
      (const $ not . null . multipleInheritances)
      defaultProperties { hasMultipleInheritances = Just True }
    generateProperty
      "no multipleInheritances"
      (const $ null . multipleInheritances)
      defaultProperties { hasMultipleInheritances = Just False }
    generateProperty
      "inheritanceCycles"
      (const $ not . null . inheritanceCycles)
      defaultProperties { hasNonTrivialInheritanceCycles = True }
    generateProperty
      "no inheritanceCycles"
      (const $ null . inheritanceCycles)
      defaultProperties { hasNonTrivialInheritanceCycles = False }
    generateProperty
      "anyThickEdge"
      (curry $ anyThickEdge . uncurry fromEdges)
      defaultProperties { hasThickEdges = Just True }
    generateProperty
      "not anyThickEdge"
      (curry $ not . anyThickEdge . uncurry fromEdges)
      defaultProperties { hasThickEdges = Just False }
    generateProperty
      "compositionCycles"
      (const $ not . null . compositionCycles)
      defaultProperties { hasCompositionCycles = True }
    generateProperty
      "no compositionCycles"
      (const $ null . compositionCycles)
      defaultProperties {
        hasCompositionCycles = False,
        hasNonTrivialInheritanceCycles = False,
        hasReverseInheritances = False
        }

generateProperty
  :: String
  -> ([String] -> [DiagramEdge] -> Bool)
  -> RelationshipProperties
  -> SpecWith ()
generateProperty property satisfies relProps =
  it ("generates " ++ property) $
    ioProperty $ do
      c <- generateCd Nothing classConfig relProps (Just 1000) Nothing
      return $ c `shouldSatisfy` uncurry satisfies

classConfig :: ClassConfig
classConfig = ClassConfig {
  classLimits        = (4, 4),
  aggregationLimits  = (0, Just 2),
  associationLimits  = (0, Just 2),
  compositionLimits  = (0, Just 3),
  inheritanceLimits  = (0, Just 3),
  relationshipLimits = (2, Just 3)
  }
