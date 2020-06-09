module Modelling.PetriNet.DiagramSpec where

import Modelling.PetriNet.Alloy          (petriNetRnd)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Parser         (prepNodes)
import Modelling.PetriNet.Types          (defaultBasicConfig,defaultAdvConfig)

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Diagrams.Backend.SVG              (renderSVG)
import Diagrams.Prelude                  (mkWidth)
import Language.Alloy.Call               (getInstances)
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $ do
      list <- getInstances (Just 1) 
           (petriNetRnd defaultBasicConfig defaultAdvConfig)
      case prepNodes "tokens" (head list) of
        Left nerror -> error nerror
        Right nodes -> do 
          dia <- drawNet "flow" nodes (head list) TwoPi
          renderSVG "test.svg" (mkWidth 200) dia `shouldReturn` ()
