module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Types (
  defaultBasicConfig,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "petriScopeBitWidth" $
    context "computes the needed bit width for generating Petri nets with Alloy" $
      it "taking some values out of the user's input" $
        petriScopeBitWidth defaultBasicConfig `shouldSatisfy` (< 7)
  describe "petriScopeMaxSeq" $
    context "computes the maximal needed space for generating Petri nets with Alloy" $
      it "taking some values out of the user's input" $
        petriScopeMaxSeq defaultBasicConfig `shouldSatisfy` (< 10)
