module Modelling.PetriNet.DiagramSpec where

import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Types

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Diagrams.Backend.SVG              (renderSVG)
import Diagrams.Prelude                  (mkWidth)
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $ do
      dia <- drawNet defaultPetri TwoPi
      renderSVG "test.svg" (mkWidth 200) dia `shouldReturn` ()
