module Modelling.ActivityDiagram.EnterASSpec where

import Modelling.ActivityDiagram.EnterAS (EnterASConfig(..), checkEnterASConfig, defaultEnterASConfig)

import Modelling.ActivityDiagram.Config (ADConfig(maxObjectNodes), defaultADConfig)
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkEnterASConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkEnterASConfig defaultEnterASConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with necessary changes" $
      checkEnterASConfig defaultEnterASConfig {
        adConfig=defaultADConfig{maxObjectNodes = 0},
        objectNodeOnEveryPath = Just True
      } `shouldSatisfy` isJust