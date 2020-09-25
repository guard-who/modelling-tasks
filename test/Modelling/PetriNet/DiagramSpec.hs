module Modelling.PetriNet.DiagramSpec where

import Modelling.PetriNet.Alloy          (petriNetRnd)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Types          (defaultBasicConfig,defaultAdvConfig)
import Modelling.PetriNet.Parser         (parsePetriLike)

import Control.Monad                     ((<=<))
import Control.Monad.Trans.Class         (lift)
import Control.Monad.Trans.Except        (ExceptT, except, runExceptT)
import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Diagrams.Backend.SVG             (renderSVG)
import Diagrams.Prelude                  (mkWidth)
import Language.Alloy.Call               (getInstances)
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $
      failOnErrors $ do
        (inst:_) <- lift $ getInstances (Just 1)
           (petriNetRnd defaultBasicConfig defaultAdvConfig)
        pl <- except $ parsePetriLike "flow" "tokens" inst
        dia <- drawNet pl TwoPi
        lift $ renderSVG "test.svg" (mkWidth 200) dia `shouldReturn` ()

failOnErrors :: ExceptT String IO a -> IO a
failOnErrors = either fail return <=< runExceptT
