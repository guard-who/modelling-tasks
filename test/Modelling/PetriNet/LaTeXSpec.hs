module Modelling.PetriNet.LaTeXSpec where

import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Types         (defaultPetri)
import Modelling.PetriNet.BasicNetFunctions (renderPdfFile)
import Test.Hspec
import Text.LaTeX                       (renderFile)

spec :: Spec
spec =
  describe "uebung" $
    context "takes a parsed PetriNet, chosen Task and Task-Type" $
      it " and creates coresponding LaTeX-Data for the mathematical notation" $ do
        out <- renderFile "test.tex" (uebung defaultPetri 1 True) >> renderPdfFile "test.tex"
        out `shouldContain` "Output written on test.pdf"

