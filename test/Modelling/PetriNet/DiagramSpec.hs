module Modelling.PetriNet.DiagramSpec where

import Modelling.PetriNet.Alloy          (petriNetRnd)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Types          (defaultBasicConfig,defaultAdvConfig)

import Control.Monad.Trans.Class         (lift)
import Control.Monad.Trans.Except        (ExceptT, runExceptT)
import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))
import Diagrams.Backend.Rasterific       (renderPdf)
import Diagrams.Prelude                  (mkWidth)
import Language.Alloy.Call               (getInstances)
import Test.Hspec

spec :: Spec
spec =
  describe "drawNet" $
    it "turns a PetriNet with a GraphvizCommand into a Diagram" $
      failOnErrors $ do
        list <- lift $ getInstances (Just 1)
           (petriNetRnd defaultBasicConfig defaultAdvConfig)
        dia <- drawNet "flow" "tokens" (head list) TwoPi
        lift $ renderPdf 300 300 "test.pdf" (mkWidth 200) dia `shouldReturn` ()

failOnErrors :: ExceptT String IO a -> IO a
failOnErrors = (>>= either fail return) . runExceptT
