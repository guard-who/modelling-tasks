module AD_SelectPetriSpec where

import AD_SelectPetri (SelectPetriConfig(..), checkSelectPetriConfig, defaultSelectPetriConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import AD_Config (ADConfig(minActions, forkJoinPairs), defaultADConfig)


spec :: Spec
spec =
  describe "checkSelectPetriConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectPetriConfig defaultSelectPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkSelectPetriConfig defaultSelectPetriConfig
          {adConfig=defaultADConfig{minActions=0, forkJoinPairs=0}, avoidAddingSinksForFinals=Just True}
            `shouldSatisfy` isJust