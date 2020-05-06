module CheckInputSpec where

import Inputs
import Types
import Test.Hspec

spec :: Spec 
spec = do
  describe "checkInput" $ do
    it "checks if the input is in given boundaries" $ do
      (checkInput defaultInput) `shouldBe` Nothing
    context "when provided with Input out of the constraints" $ do
      it "it returns a String with nessecary changes" $ do
        checkInput defaultInput{places = 0} `shouldBe` Just "There must at least be 1 Place"
      
      