module Modelling.ActivityDiagram.MatchPetriSpec where

import Modelling.ActivityDiagram.MatchPetri (MatchPetriConfig(..), checkMatchPetriConfig, defaultMatchPetriConfig, matchPetriAlloy, extractSupportSTs)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (ADConfig(minActions, forkJoinPairs, decisionMergePairs, cycles), defaultADConfig)

import Modelling.ActivityDiagram.Alloy(getAlloyInstancesWith)
import Modelling.ActivityDiagram.Instance(parseInstance)

import Modelling.ActivityDiagram.Petrinet(convertToPetrinet)

spec :: Spec
spec = do
  describe "checkADConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkMatchPetriConfig defaultMatchPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkMatchPetriConfig defaultMatchPetriConfig
          {adConfig=defaultADConfig{minActions=0, forkJoinPairs=0}, avoidAddingSinksForFinals=Just True}
            `shouldSatisfy` isJust
  describe "matchPetriAlloy" $ do
    context "when supportSTAbsent is set to Just False" $
      it "it returns an Alloy Specification from which only diagrams which contain support STs are generated" $ do
        let spec = matchPetriAlloy defaultMatchPetriConfig {supportSTAbsent = Just False}
        inst <- getAlloyInstancesWith (Just 50) spec
        let ad = map (failWith id . parseInstance "this" "this") inst
        all (hasSupportSTs . convertToPetrinet) ad `shouldBe` (True::Bool)
    context "when supportSTAbsent is set to Just True" $
      it "it returns an Alloy Specification from which only diagrams which contain no support STs are generated" $ do
        let spec = matchPetriAlloy defaultMatchPetriConfig {adConfig=defaultADConfig{cycles=0, decisionMergePairs=1}, supportSTAbsent=Just True}
        inst <- getAlloyInstancesWith (Just 50) spec
        let ad = map (failWith id .parseInstance "this" "this") inst
        any (hasSupportSTs . convertToPetrinet) ad `shouldBe` (False::Bool)
  where hasSupportSTs = not . null . extractSupportSTs

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id