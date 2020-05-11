module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types

import Test.Hspec
import Data.Maybe

spec :: Spec 
spec =
  describe "checkInput" $ do
    it "checks if the input is in given boundaries" $ 
      checkConfig defaultPetriConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkConfig defaultPetriConfig{places = 0} `shouldSatisfy` isJust
