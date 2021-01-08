module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Types (
  defaultBasicConfig,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "petriScopeBitwidth" $
    context "computes the needed Bitwidth for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScopeBitwidth defaultBasicConfig `shouldSatisfy` (< 7)
  describe "petriScopeMaxSeq" $
    context "computes the maximal needed Space for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScopeMaxSeq defaultBasicConfig `shouldSatisfy` (< 10)
