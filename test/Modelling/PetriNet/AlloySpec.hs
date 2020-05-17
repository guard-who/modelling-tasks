module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Alloy
import Modelling.PetriNet.Parser (convertPetri)
import Modelling.PetriNet.Types         
  (defaultPetriBasicConfig,defaultPetriTask1Config,Petri)

import Test.Hspec
import Language.Alloy.Call       (existsInstance,getInstances)

spec :: Spec
spec = 
  describe "renderFalse" $
    context "giving it the Users Input and resulting right PetriNet" $
      it "creates a false possible Answer for the given task" $ do
        petri <- prepPetri
        let falseAlloy = renderFalse petri defaultPetriTask1Config 
        existsInstance falseAlloy `shouldReturn` True
  -- describe "petriScope" $
    -- context "compute a scope for generating Petrinets with Alloy" $
      -- it "taking some values out of the User's input" $
        -- petriScope defaultPetriBasicConfig `shouldSatisfy` (< 10)
        
prepPetri :: IO(Petri)
prepPetri = do
  list <- getInstances (Just 1) (petriNetRnd defaultPetriBasicConfig)
  let out = convertPetri "flow" "tokens" (head list)
  case out of
    Left merror -> error merror
    Right petri -> return petri
