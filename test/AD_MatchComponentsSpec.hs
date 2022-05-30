module AD_MatchComponentsSpec where

import AD_MatchComponents (MatchPetriConfig(..), checkMatchPetriConfig, defaultMatchPetriConfig, matchPetriAlloy)

import qualified Data.Map as M ((!), null, keys)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import AD_Config (ADConfig(minActions, forkJoinPairs, decisionMergePairs, cycles), defaultADConfig)

import AD_Alloy(getAlloyInstancesWith)
import AD_Instance(parseInstance)

import AD_Petrinet(convertToPetrinet, PetriKey(..))
import Modelling.PetriNet.Types (PetriLike(..), Node(..))

spec :: Spec
spec = do
  describe "checkADConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkMatchPetriConfig defaultMatchPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkMatchPetriConfig defaultMatchPetriConfig
          {adConfig=defaultADConfig{minActions=0, forkJoinPairs=0}, avoidAddingSinksForFinals=Just True}
            `shouldSatisfy` isJust
  describe "matchPetriAlloy" $ do
    context "when supportSTExist is set to Just True" $
      it "it returns an Alloy Specification from which only diagrams which contain support STs are generated" $ do
        let spec = matchPetriAlloy defaultMatchPetriConfig {supportSTExist = Just True}
        inst <- getAlloyInstancesWith (Just 50) spec
        let ad = map (failWith id . parseInstance "this" "this") inst
        all (hasSupportSTs . convertToPetrinet) ad `shouldBe` (True::Bool)
    context "when supportSTExist is set to Just False" $
      it "it returns an Alloy Specification from which only diagrams which contain no support STs are generated" $ do
        let spec = matchPetriAlloy defaultMatchPetriConfig {adConfig=defaultADConfig{cycles=0, decisionMergePairs=1}, supportSTExist=Just False}
        inst <- getAlloyInstancesWith (Just 50) spec
        let ad = map (failWith id .parseInstance "this" "this") inst
        any (hasSupportSTs . convertToPetrinet) ad `shouldBe` (False::Bool)


failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

hasSupportSTs :: PetriLike PetriKey -> Bool
hasSupportSTs petri =  any (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False