module Modelling.PetriNet.DiagramSpec where

import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Types

import Control.Monad
import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $
      void (drawNet defaultPetri TwoPi) `shouldReturn` ()
