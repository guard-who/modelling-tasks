module Modelling.PetriNet.BasicNetFunctionsSpec where

import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types
  (defaultBasicConfig,BasicConfig(..),defaultChangeConfig,ChangeConfig(..))       

import Test.Hspec
import Data.Maybe

spec :: Spec 
spec = do
  describe "checkBasicConfig" $ do
    it "checks if the basic Input is in given boundaries" $ 
      checkBasicConfig defaultBasicConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkBasicConfig defaultBasicConfig{places = 0} 
          `shouldSatisfy` isJust
  describe "checkChangeConfig" $ do
    it "checks if the input for Changes is in given boundaries" $
      checkChangeConfig defaultBasicConfig defaultChangeConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkChangeConfig defaultBasicConfig defaultChangeConfig{tokenChangeOverall = -1}
          `shouldSatisfy` isJust