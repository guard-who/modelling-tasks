module Modelling.ActivityDiagram.FindAuxiliaryPetriNodesSpec where

import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  FindAuxiliaryPetriNodesConfig (..),
  checkFindAuxiliaryPetriNodesConfig,
  defaultFindAuxiliaryPetriNodesConfig,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, forkJoinPairs),
  defaultAdConfig,
  )


spec :: Spec
spec =
  describe "checkFindAuxiliaryPetriNodesConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkFindAuxiliaryPetriNodesConfig defaultFindAuxiliaryPetriNodesConfig
        `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkFindAuxiliaryPetriNodesConfig defaultFindAuxiliaryPetriNodesConfig {
          adConfig = defaultAdConfig {actionLimits = (0, 4), forkJoinPairs = 0},
          presenceOfSinkTransitionsForFinals = Just False
          }
            `shouldSatisfy` isJust
