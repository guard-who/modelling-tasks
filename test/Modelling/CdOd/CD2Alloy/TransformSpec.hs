{-# LANGUAGE QuasiQuotes #-}
module Modelling.CdOd.CD2Alloy.TransformSpec where

import Modelling.CdOd.CD2Alloy.Transform (
  LinguisticReuse (..),
  ExtendsAnd (..),
  combineParts,
  transform,
  )
import Modelling.CdOd.Types (
  ObjectConfig (..),
  ObjectProperties (..),
  )
import Modelling.Common                 (withUnitTestsUsingPath)

import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
import Test.Hspec
import Test.Similarity                  (debugAssertEqual)

spec :: Spec
spec = do
  testTransform None
  testTransform $ ExtendsAnd NothingMore

testTransform :: LinguisticReuse -> Spec
testTransform linguisticReuse =
  withUnitTestsUsingPath
    [i|transform using #{linguisticReuse}|]
    does
    (dir ++ filter (/= ' ') (show linguisticReuse))
    "als" $
    \file -> flip (debugAssertEqual (Just file) "") . getResult . read
  where
    does = "generates expected Alloy code"
    dir = "test/unit/Modelling/CdOd/CD2Alloy/Transform/"
    getResult cd = combineParts $ transform
      linguisticReuse
      cd
      Nothing
      []
      objectConfig
      objectProperties
      "1"
      "-"
    objectConfig = ObjectConfig {
      linkLimits           = (4, Just 10),
      linksPerObjectLimits = (0, Just 4),
      objectLimits         = (2, 4)
      }
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 1 % 3,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Just True,
      usesEveryRelationshipName = Nothing
      }
