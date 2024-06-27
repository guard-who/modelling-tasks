module Modelling.PetriNet.DiagramSpec where

import Capabilities.Diagrams.IO          ()
import Capabilities.Graphviz.IO          ()
import Modelling.Auxiliary.Common        (Object)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.MatchToMath    (petriNetRnd)
import Modelling.PetriNet.Types (
  DrawSettings (..),
  SimplePetriLike,
  defaultAdvConfig,
  defaultBasicConfig,
  )
import Modelling.PetriNet.Parser         (parseNet)

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Diagrams.Backend.SVG             (renderSVG)
import Diagrams.Prelude                  (mkWidth)
import Language.Alloy.Call               (getInstances)
import System.IO.Extra                   (withTempFile)
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $
      do
        (inst:_) <- getInstances (Just 1)
           (petriNetRnd defaultBasicConfig defaultAdvConfig)
        pl <- parseNet "flow" "tokens" inst
        dia <- drawNet show (pl :: SimplePetriLike Object) DrawSettings {
          withPlaceNames = True,
          withSvgHighlighting = True,
          withTransitionNames = False,
          with1Weights = False,
          withGraphvizCommand = TwoPi
          }
        withTempFile $ \f -> renderSVG f (mkWidth 200) dia `shouldReturn` ()
