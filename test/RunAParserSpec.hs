module RunAParserSpec where 

import PetriAlloy
import PetriParser
import Types
import Language.Alloy.Call
import Test.Hspec

spec :: Spec
spec =
  describe "runAParser" $
    context "out of a given AlloyInstance" $
      it "creates a false Answer for Tasktype 1 with the connected changes to the original" $ do
        list <- getInstances (Just 1) (petriNetRnd defaultInput)
        runAParser (head list) `shouldSatisfy` const True