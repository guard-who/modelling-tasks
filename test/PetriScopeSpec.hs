{-# LANGUAGE NamedFieldPuns #-}

module PetriScopeSpec where

import PetriAlloy
import Types
import Test.Hspec

spec :: Spec 
spec =
  describe "petriScope" $
    context "compute a scope for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScope defaultInput `shouldSatisfy` const True