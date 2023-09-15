module Modelling.CdOd.CD2Alloy.TransformSpec where

import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  transform,
  )
import Modelling.CdOd.Types (
  ObjectConfig (..),
  ObjectProperties (..),
  )
import Modelling.Common                 (withUnitTestsUsingPath)

import Test.Hspec
import Test.Similarity

spec :: Spec
spec = do
  withUnitTestsUsingPath "transform" does dir "als" $
    \file -> flip (debugAssertEqual (Just file) "") . getResult . read
  where
    does = "generates expected Alloy code"
    dir = "test/unit/Modelling/CdOd/CD2Alloy/Transform/"
    getResult cd = combineParts
      $ transform cd [] objectConfig objectProperties "1" "-"
    objectConfig = ObjectConfig {
      linkLimits           = (4, Just 10),
      linksPerObjectLimits = (0, Just 4),
      objectLimits         = (2, 4)
      }
    objectProperties = ObjectProperties {
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Just True,
      usesEveryRelationshipName = Nothing
      }
