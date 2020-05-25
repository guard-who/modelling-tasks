module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types       
  (defaultMathConfig,MathConfig(..),defaultChangeConfig,ChangeConfig(..))

import Test.Hspec

spec :: Spec 
spec =
  describe "matchToMath" $
    context "creates, out of a given Config and a Boolean for Tasktype," $
      it "everything needed to create the Task" $ do
        (_,_,changes) <- matchToMath True defaultMathConfig
             {changeTask = (changeTask defaultMathConfig)
               {tokenChangeOverall = 1,maxTokenChangePerPlace = 2}}
        case changes of
          Right dChng -> print (map snd dChng) `shouldReturn` ()
          Left tChng -> print (map snd tChng) `shouldReturn` ()
        


