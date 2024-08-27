module Modelling.ActivityDiagram.FindSupportPetriNodeSpec where

import Modelling.ActivityDiagram.FindSupportPetriNode (
  FindSupportPetriNodeConfig(..),
  checkFindSupportPetriNodeConfig,
  defaultFindSupportPetriNodeConfig,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, forkJoinPairs),
  defaultAdConfig,
  )


spec :: Spec
spec =
  describe "checkFindSupportPetriNodeConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkFindSupportPetriNodeConfig defaultFindSupportPetriNodeConfig
        `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkFindSupportPetriNodeConfig defaultFindSupportPetriNodeConfig {
          adConfig = defaultAdConfig {actionLimits = (0, 4), forkJoinPairs = 0},
          avoidAddingSinksForFinals = Just True
          }
            `shouldSatisfy` isJust
