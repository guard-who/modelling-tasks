module Modelling.ActivityDiagram.MatchAdSpec where

import Modelling.ActivityDiagram.MatchAd (
  MatchAdConfig (..),
  checkMatchAdConfig,
  defaultMatchAdConfig,
  )

import Modelling.ActivityDiagram.Config (
  AdConfig (activityFinalNodes),
  defaultAdConfig,
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkMatchAdConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkMatchAdConfig defaultMatchAdConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with necessary changes" $
      checkMatchAdConfig defaultMatchAdConfig {
        adConfig=defaultAdConfig{activityFinalNodes = 2},
        withActivityFinalInForkBlocks = Just False
      } `shouldSatisfy` isJust
