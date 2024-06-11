module Modelling.ActivityDiagram.ConfigSpec where

import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)


spec :: Spec
spec = do
  describe "checkAdConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkAdConfig defaultAdConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (0, 4),
          objectNodeLimits = (0, 1)
          }
          `shouldSatisfy` isJust
