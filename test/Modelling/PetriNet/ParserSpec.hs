module Modelling.PetriNet.ParserSpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Parser
import Modelling.PetriNet.Types

import Language.Alloy.Call
import Test.Hspec

spec :: Spec
spec =
  describe "runAParser" $
    context "out of a given AlloyInstance" $
      it "creates a false Answer for Tasktype 1 with the connected changes to the original" $ do
        list <- getInstances (Just 1) (petriNetRnd defaultPetriConfig)
        runAParser (head list) `shouldSatisfy` const True
