module Modelling.PetriNet.LaTeXSpec where

import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Types         (defaultPetri)

import Test.Hspec

spec :: Spec
spec =
  describe "runTex" $
    context "after parsing a PetriNet out of the Input and choosing a task" $
      it "creates LaTeX-Data out of the given parts" $
        runTex defaultPetri 1 `shouldReturn` ()
