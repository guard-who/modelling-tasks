module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types

import Control.Monad  (void)
import Test.Hspec
import Data.Maybe

spec :: Spec 
spec = do
  describe "checkTask1Input" $ do
    it "checks if the input for Task1 is in given boundaries" $
      checkTask1Config defaultPetriTask1Config `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkTask1Config defaultPetriTask1Config{tokenChangeOverall = -1} 
          `shouldSatisfy` isJust
  describe "matchToMath" $
    context "out of a given Task1 Config and a Boolean for Tasktype" $
      it "everything needed to create the Task is generated" $ 
        void (matchToMath True defaultPetriTask1Config) `shouldReturn` ()
