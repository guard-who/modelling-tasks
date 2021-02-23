module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types         (ChangeConfig(..))

import Control.Monad.Random             (Random (random, randomR))
import Control.Monad.Trans.Except       (runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import System.Random                    (getStdGen)
import Test.Hspec

import Modelling.PetriNet.TestCommon (
  firstInstanceConfig,
  )

spec :: Spec 
spec = do
  describe "matchToMath" $ do
    defaultMathTask id (const ()) mathToGraph
    defaultMathTask (const ()) id graphToMath
  describe "checkConfig" $
    it "checks if the input is in given boundaries for the task" $
      checkMathConfig defaultMathConfig `shouldBe` Nothing
  where
    defaultMathTask f g task =
      context "using its default config" $
        it "generates everything needed to create the Task" $ do
          gen <- getStdGen
          let seed = fst $ random gen
              section = fst $ randomR (0, 3) gen
          Right matchInst <- runExceptT $ task defaultMathConfig {
            changeConfig = (changeConfig defaultMathConfig) {
                tokenChangeOverall = 1,
                maxTokenChangePerPlace = 1
                },
            alloyConfig = firstInstanceConfig
            } section seed
          print (bimap f g matchInst) `shouldReturn` ()
