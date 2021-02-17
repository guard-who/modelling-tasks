module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types         (ChangeConfig(..))

import Control.Monad.Trans.Except       (runExceptT)
import Test.Hspec

spec :: Spec 
spec = do
  describe "matchToMath" $ do
    defaultMathTask mathToGraph 1
    defaultMathTask graphToMath 1
  describe "checkConfig" $
    it "checks if the input is in given boundaries for the task" $
      checkConfig defaultMathConfig `shouldBe` Nothing
  where
    defaultMathTask task x =
      context ("using its default config and " ++ show x) $
        it "generates everything needed to create the Task" $ do
          Right (_, _, changes) <- runExceptT $ task defaultMathConfig {
            changeConfig = (changeConfig defaultMathConfig) {
                tokenChangeOverall = 1,
                maxTokenChangePerPlace = 1
                }
            } 0 x
          print (map snd changes) `shouldReturn` ()
