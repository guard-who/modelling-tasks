module Modelling.CdOd.GenerateSpec where

import Modelling.CdOd.Edges (
  anyMarkedEdge,
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
    it "generates non trivial inheritance instances" $
      ioProperty $ do
        c <- generateCd (Just False) classConfig defaultProperties (Just 1000) Nothing
        return $ c `shouldSatisfy` not . uncurry hasAssociationAtOneSuperclass
    generateProperty
      "hasWrongLimits"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongAssocs = 1 }
    generateProperty
      "hasWrongLimits"
      (const $ not . null . wrongLimits)
      defaultProperties { wrongCompositions = 1 }
    generateProperty
      "hasSelfEdges"
      (const $ not . null . selfEdges)
      defaultProperties { selfRelationships = 1 }
    generateProperty
      "hasSelfEdges"
      (const $ not . null . selfEdges)
      defaultProperties { selfInheritances = 1 }
    generateProperty
      "hasDoubleConnections"
      (const $ not . null . doubleConnections)
      defaultProperties { hasDoubleRelationships = True }
    generateProperty
      "hasDoubleConnections"
      (const $ not . null . doubleConnections)
      defaultProperties { hasReverseRelationships = True }
    generateProperty
      "hasMultipleInheritances"
      (const $ not . null . multipleInheritances)
      defaultProperties { hasMultipleInheritances = True }
    generateProperty
      "hasInheritanceCycles"
      (const $ not . null . inheritanceCycles)
      defaultProperties { hasNonTrivialInheritanceCycles = True }
    generateProperty
      "anyMarkedEdge"
      (curry $ anyMarkedEdge . uncurry fromEdges)
      defaultProperties { hasMarkedEdges = Just True }

generateProperty
  :: String
  -> ([String] -> [DiagramEdge] -> Bool)
  -> RelationshipProperties
  -> SpecWith ()
generateProperty property satisfies relProps =
  it ("abides to property " ++ property) $
    ioProperty $ do
      c <- generateCd Nothing classConfig relProps (Just 1000) Nothing
      return $ c `shouldSatisfy` uncurry satisfies

classConfig :: ClassConfig
classConfig = ClassConfig {
  classes      = (4, 4),
  aggregations = (0, Just 2),
  associations = (0, Just 2),
  compositions = (0, Just 1),
  inheritances = (1, Just 3)
  }
