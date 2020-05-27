module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Parser (convertPetri)
import Modelling.PetriNet.Types         
  (defaultBasicConfig,defaultMathConfig,defaultAdvConfig,Petri)

import Test.Hspec
import Language.Alloy.Call       (existsInstance,getInstances)

spec :: Spec
spec = do
  describe "renderFalse" $
    context "giving it the Users Input and resulting right PetriNet" $
      it "creates a false possible Answer for the given task" $ do
        petri <- prepPetri
        let falseAlloy = renderFalse petri defaultMathConfig 
        existsInstance falseAlloy `shouldReturn` True
  describe "petriScopeBitwidth" $
    context "computes the needed Bitwidth for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScopeBitwidth defaultBasicConfig `shouldSatisfy` (< 7)
  describe "petriScopeMaxSeq" $
    context "computes the maximal needed Space for generating Petrinets with Alloy" $
      it "taking some values out of the User's input" $
        petriScopeMaxSeq defaultBasicConfig `shouldSatisfy` (< 10)
        
prepPetri :: IO Petri
prepPetri = do
  list <- getInstances (Just 1) 
           (petriNetRnd defaultBasicConfig defaultAdvConfig)
  let out = convertPetri "flow" "tokens" (head list)
  case out of
    Left merror -> error merror
    Right petri -> return petri
