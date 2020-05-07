module PetriAlloySpec where

import PetriAlloy

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "removeLines" $ do
    it "preserves the string (besides newlines) when removing 0 lines" $
      property $ \x -> let newline
                             | not (null x) && last x /= '\n' = "\n"
                             | otherwise                      = ""
                       in removeLines 0 x == x ++ newline
    it "may shorten the string when removing more then 0 lines" $
      property $ \n x -> n > 0 ==> length (removeLines n x) <= length x
    it "removes the given number of lines (according to lines function)" $
      property $ \n x -> let len = length (lines x)
                             n'  = abs (n `div` 10)
                         in n' <= len
                            ==> len == length (lines $ removeLines n' x) + n'
