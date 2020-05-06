module RunAParserSpec where 

import PetriAlloy
import PetriParser
import Types
import Language.Alloy.Call
import Test.Hspec

spec :: Spec
spec = do
  describe "runAParser" $ do
    context "out of a given AlloyInstance" $ do
      it "creates a false Answer for Tasktype 1 with the connected changes to the original" $ do
        list <- getInstances (Just 1) (petriNetRnd defaultInput)
        runAParser (head list) `shouldSatisfy` const True