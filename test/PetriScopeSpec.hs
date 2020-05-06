{-# LANGUAGE NamedFieldPuns #-}

module PetriScopeSpec where

import PetriAlloy
import Types
import Test.Hspec

spec :: Spec 
spec = do
  describe "petriScope" $ do
    context "compute a scope for generating Petrinets with Alloy" $ do
      it "taking some values out of the User's input" $
        petriScope defaultInput `shouldSatisfy` const True