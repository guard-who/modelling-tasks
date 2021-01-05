module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types       
  (defaultMathConfig,MathConfig(..),ChangeConfig(..))

import Control.Monad.Trans.Except       (runExceptT)
import Test.Hspec

spec :: Spec 
spec = do
  describe "matchToMath" $ do
    defaultMathTask True
    defaultMathTask False
  describe "checkConfig" $
    it "checks if the input is in given boundaries for the task" $
      checkConfig defaultMathConfig `shouldBe` Nothing
  where
    defaultMathTask bool =
      context ("using its default config and " ++ show bool) $
        it "generates everything needed to create the Task" $ do
          Right (_,_,changes) <- runExceptT $ matchToMath 0 bool defaultMathConfig
               {changeTask = (changeTask defaultMathConfig)
                 {tokenChangeOverall = 1,maxTokenChangePerPlace = 1}}
          case (bool, changes) of
            (False, Right dChng) -> print (map snd dChng) `shouldReturn` ()
            (True , Left tChng ) -> print (map snd tChng) `shouldReturn` ()
            _                    -> error "unexpected task type"
