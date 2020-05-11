module Modelling.PetriNet.LaTeXSpec where

import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Types         (defaultPetri)

import Test.Hspec

spec :: Spec
spec =
  describe "createPetriTex" $
    context "takes a parsed PetriNet" $
      it " and creates coresponding LaTeX-Data for the mathematical notation" $
        createPetriTex defaultPetri `shouldSatisfy` const True
