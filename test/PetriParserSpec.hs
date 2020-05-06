module PetriParserSpec where

import PetriParser
import Test.Hspec

spec :: Spec
spec = do
  describe "listTill" $ do
    it "gives you the List up to the first occurence of an Element" $
      listTill "This is a  Test" 'a' `shouldBe` "This is "
    it "can do it with other kinds of Lists, too" $
      listTill [3,1,2,5,4] 5 `shouldBe` [3,1,2]