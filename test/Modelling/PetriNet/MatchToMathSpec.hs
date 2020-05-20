module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types

import Test.Hspec
import Data.Maybe

spec :: Spec 
spec = do
  describe "checkTask1Input" $ do
    it "checks if the input for Task1 is in given boundaries" $
      checkMathConfig defaultMathConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkMathConfig defaultMathConfig
              {changeTask = defaultChangeConfig{tokenChangeOverall = -1}}
          `shouldSatisfy` isJust
  describe "matchToMath" $
    context "out of a given Task1 Config and a Boolean for Tasktype" $
      it "everything needed to create the Task is generated" $ do
        (dia,tex,changes) <- matchToMath True defaultMathConfig
             {changeTask = defaultChangeConfig
               {tokenChangeOverall = 1,maxTokenChangePerPlace = 2}}
        case changes of
          Right dChng -> print (get2ndElements dChng) `shouldReturn` ()
          Left tChng -> print (get2ndElements tChng) `shouldReturn` ()
        

get2ndElements :: [(a,b)] -> [b]
get2ndElements list = [x| (y,x) <- list]
