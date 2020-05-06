module RunTexSpec where

import PetriTex
import Types       (defaultPetri)
import Test.Hspec

spec :: Spec
spec = do
  describe "runTex" $ do
    context "after parsing a PetriNet out of the Input and choosing a task" $ do
      it "creates LaTeX-Data out of the given parts" $
        runTex defaultPetri 1 `shouldReturn` ()