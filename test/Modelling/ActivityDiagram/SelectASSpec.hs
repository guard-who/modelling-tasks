module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..), checkSelectASConfig, defaultSelectASConfig)

import Modelling.ActivityDiagram.Config (ADConfig(maxObjectNodes), defaultADConfig)
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkSelectASConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkSelectASConfig defaultSelectASConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with necessary changes" $
      checkSelectASConfig defaultSelectASConfig {
        adConfig=defaultADConfig{maxObjectNodes = 0},
        objectNodeOnEveryPath = Just True
      } `shouldSatisfy` isJust