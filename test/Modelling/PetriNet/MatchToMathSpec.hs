module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types       
  (defaultMathConfig,MathConfig(..),ChangeConfig(..))

import Test.Hspec

spec :: Spec 
spec = do
  describe "matchToMath" $
    context "creates, out of a given Config and a Boolean for Tasktype," $
      it "everything needed to create the Task" $ do
        (_,_,changes) <- matchToMath 0 True defaultMathConfig
             {changeTask = (changeTask defaultMathConfig)
               {tokenChangeOverall = 1,maxTokenChangePerPlace = 1}}
        case changes of
          Right dChng -> print (map snd dChng) `shouldReturn` ()
          Left tChng -> print (map snd tChng) `shouldReturn` ()
  describe "checkConfig" $ 
    it "checks if the input is in given boundaries for the task" $
      checkConfig defaultMathConfig `shouldBe` Nothing
        


