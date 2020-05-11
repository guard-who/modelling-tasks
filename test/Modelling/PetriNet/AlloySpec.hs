module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Types         (defaultPetriConfig,defaultPetri)

import Test.Hspec

spec :: Spec
spec = do
  describe "renderFalse" $
    context "giving it the Users Input and resulting right PetriNet" $
      it "creates a false possible Answer for the given task" $
        renderFalse defaultPetri defaultPetriConfig `shouldSatisfy` ((> 0) . length)
  describe "petriScope" $
    context "compute a scope for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScope defaultPetriConfig `shouldSatisfy` (< 10)
