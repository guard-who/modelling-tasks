module Modelling.CdOd.GenerateSpec where

import Modelling.CdOd.Edges (
  anyMarkedEdge,
  compositionCycles,
  doubleConnections,
  fromEdges,
  hasAssociationAtOneSuperclass,
  inheritanceCycles,
  multipleInheritances,
  selfEdges,
  wrongLimits,
  )
import Modelling.CdOd.Generate          (generateCd)
import Modelling.CdOd.Types (
  ClassConfig (..),
  DiagramEdge,
  RelationshipProperties (..),
  defaultProperties,
  )

import Test.Hspec
import Test.QuickCheck                  (ioProperty)

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
      "wrongLimits (Assoc)"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongAssocs = 1 }
    generateProperty
      "wrongLimits (Composition)"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongCompositions = 1 }
    generateProperty
      "no wrongLimits"
      (const $ null . wrongLimits)
      defaultProperties {
        wrongAssocs = 0,
        wrongCompositions = 0
        }
    generateProperty
      "selfEdges (Assoc)"
      (const $ not . null . selfEdges)
      defaultProperties { selfRelationships = 1 }
    generateProperty
      "selfEdges (Inheritance)"
      (const $ not . null . selfEdges)
      defaultProperties { selfInheritances = 1 }
    generateProperty
      "no selfEdges"
      (const $ null . selfEdges)
      defaultProperties {
        selfInheritances = 0,
        selfRelationships = 0
        }
    generateProperty
      "doubleConnections (same direction)"
      (const $ not . null . doubleConnections)
      defaultProperties { hasDoubleRelationships = True }
    generateProperty
      "doubleConnections (reverse direction)"
      (const $ not . null . doubleConnections)
      defaultProperties { hasReverseRelationships = True }
    generateProperty
      "no doubleConnections"
      (const $ null . doubleConnections)
      defaultProperties {
        hasDoubleRelationships = False,
        hasReverseInheritances = False,
        hasReverseRelationships = False
        }
    generateProperty
      "multipleInheritances"
      (const $ not . null . multipleInheritances)
      defaultProperties { hasMultipleInheritances = True }
    generateProperty
      "no multipleInheritances"
      (const $ null . multipleInheritances)
      defaultProperties { hasMultipleInheritances = False }
    generateProperty
      "inheritanceCycles"
      (const $ not . null . inheritanceCycles)
      defaultProperties { hasNonTrivialInheritanceCycles = True }
    generateProperty
      "no inheritanceCycles"
      (const $ null . inheritanceCycles)
      defaultProperties { hasNonTrivialInheritanceCycles = False }
    generateProperty
      "anyMarkedEdge"
      (curry $ anyMarkedEdge . uncurry fromEdges)
      defaultProperties { hasMarkedEdges = Just True }
    generateProperty
      "not anyMarkedEdge"
      (curry $ not . anyMarkedEdge . uncurry fromEdges)
      defaultProperties { hasMarkedEdges = Just False }
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
  classes      = (4, 4),
  aggregations = (0, Just 2),
  associations = (0, Just 2),
  compositions = (0, Just 3),
  inheritances = (1, Just 3)
  }
