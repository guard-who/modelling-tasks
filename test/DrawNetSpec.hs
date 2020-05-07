module DrawNetSpec where 

import PetriDiagram
import Control.Monad
import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Types
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $
      void (drawNet (prepNet defaultPetri) TwoPi) `shouldReturn` ()