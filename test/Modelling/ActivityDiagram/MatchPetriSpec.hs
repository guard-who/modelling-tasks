module Modelling.ActivityDiagram.MatchPetriSpec where

import Modelling.ActivityDiagram.MatchPetri (
  MatchPetriConfig (..),
  checkMatchPetriConfig,
  defaultMatchPetriConfig,
  extractAuxiliaryPetriNodes,
  matchPetriAlloy,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, cycles, decisionMergePairs, forkJoinPairs),
  defaultAdConfig,
  )

import Modelling.ActivityDiagram.Instance(parseInstance)
import Modelling.ActivityDiagram.PetriNet (convertToSimple)

import Language.Alloy.Call (getInstances)

spec :: Spec
spec = do
  describe "checkAdConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkMatchPetriConfig defaultMatchPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkMatchPetriConfig defaultMatchPetriConfig {
          adConfig = defaultAdConfig {actionLimits = (0, 4), forkJoinPairs = 0},
          avoidAddingSinksForFinals = Just True
          }
            `shouldSatisfy` isJust
  describe "matchPetriAlloy" $ do
    context "when auxiliaryPetriNodeAbsent is set to Just False" $
      it "it returns an Alloy Specification from which only diagrams which contain Auxiliary PetriNodes are generated" $ do
        let spec' = matchPetriAlloy
              defaultMatchPetriConfig {auxiliaryPetriNodeAbsent = Just False}
        inst <- getInstances (Just 50) spec'
        ad <- mapM parseInstance inst
        all (hasAuxiliaryPetriNodes . convertToSimple) ad `shouldBe` True
    context "when auxiliaryPetriNodeAbsent is set to Just True" $
      it "it returns an Alloy Specification from which only diagrams which contain no Auxiliary PetriNodes are generated" $ do
        let spec' = matchPetriAlloy defaultMatchPetriConfig {
              adConfig = defaultAdConfig {cycles = 0, decisionMergePairs = 1},
              auxiliaryPetriNodeAbsent = Just True
              }
        inst <- getInstances (Just 50) spec'
        ad <- mapM parseInstance inst
        any (hasAuxiliaryPetriNodes . convertToSimple) ad `shouldBe` False
  where hasAuxiliaryPetriNodes = not . null . extractAuxiliaryPetriNodes
