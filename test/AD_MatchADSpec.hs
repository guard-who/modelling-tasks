module AD_MatchADSpec where

import AD_MatchAD (MatchADConfig(..), checkMatchADConfig, defaultMatchADConfig)

import AD_Config (ADConfig(activityFinalNodes), defaultADConfig)
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkMatchADConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkMatchADConfig defaultMatchADConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with necessary changes" $
      checkMatchADConfig defaultMatchADConfig {
        adConfig=defaultADConfig{activityFinalNodes = 2},
        noActivityFinalInForkBlocks=Just True
      } `shouldSatisfy` isJust