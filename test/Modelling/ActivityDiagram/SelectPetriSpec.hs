module Modelling.ActivityDiagram.SelectPetriSpec where

import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..), checkSelectPetriConfig, defaultSelectPetriConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, forkJoinPairs),
  defaultAdConfig,
  )


spec :: Spec
spec =
  describe "checkSelectPetriConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectPetriConfig defaultSelectPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkSelectPetriConfig defaultSelectPetriConfig {
          adConfig = defaultAdConfig {
            actionLimits = (0, 4),
            forkJoinPairs = 0
            },
          avoidAddingSinksForFinals = Just True
          }
            `shouldSatisfy` isJust
