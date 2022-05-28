module AD_ConfigSpec where

import AD_Config (ADConfig(..), checkADConfig, defaultADConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkADConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkADConfig defaultADConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with nessecary changes" $
      checkADConfig defaultADConfig{minActions=0, minObjectNodes=0}
        `shouldSatisfy` isJust