module Modelling.PetriNet.LaTeXSpec where

import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Types         (defaultPetri)

import Control.Monad
import Test.Hspec
import Text.LaTeX

spec :: Spec
spec =
  describe "uebung" $
    context "takes a parsed PetriNet, chosen Task and Task-Type" $
      it " and creates coresponding LaTeX-Data for the mathematical notation" $
        renderFile "test.tex" (uebung defaultPetri 1 True) `shouldReturn` () 
