module AD_MatchComponentsSpec where

import AD_MatchComponents (MatchPetriConfig(..), checkMatchPetriConfig, defaultMatchPetriConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import AD_Config (ADConfig(minActions, forkJoinPairs), defaultADConfig)

spec :: Spec
spec = describe "checkADConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkMatchPetriConfig defaultMatchPetriConfig  `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with nessecary changes" $
      checkMatchPetriConfig defaultMatchPetriConfig
        {adConfig=defaultADConfig{minActions=0, forkJoinPairs=0}, avoidAddingSinksForFinals=Just True}
          `shouldSatisfy` isJust