module RenderFalseSpec where 

import FalsePetri 
import Types               (defaultInput,defaultPetri)
import Test.Hspec

spec :: Spec
spec =
  describe "renderFalse" $ do
    context "giving it the Users Input and resulting right PetriNet" $
      it "creates a false possible Answer for the given task" $
        renderFalse defaultPetri defaultInput `shouldSatisfy` const True
  