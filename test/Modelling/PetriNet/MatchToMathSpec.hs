{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.MatchToMathSpec where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types (
  ChangeConfig(..),
  PetriLike,
  SimpleNode,
  defaultAlloyConfig,
  maxInstances,
  )

import Control.Monad.Random             (Random (random, randomR))
import Control.Monad.Trans.Except       (runExceptT)
import Data.Either                      (isRight)
import System.Random                    (getStdGen)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchToMath" $ do
    describe "as mathToGraph" $
      defaultMathTask (mathToGraph @PetriLike @SimpleNode)
    describe "as graphToMath" $
      defaultMathTask (graphToMath @PetriLike @SimpleNode)
  describe "checkConfig" $
    it "checks if the input is in given boundaries for the task" $
      checkMathConfig defaultMathConfig `shouldBe` Nothing
  where
    defaultMathTask task =
      context "using its default config" $
        it "generates everything needed to create the Task" $ do
          gen <- getStdGen
          let seed = fst $ random gen
              section = fst $ randomR (0, 3) gen
          matchInst <- runExceptT $ task defaultMathConfig {
            changeConfig = (changeConfig defaultMathConfig) {
                tokenChangeOverall = 1,
                maxTokenChangePerPlace = 1
                },
            alloyConfig = defaultAlloyConfig {
              maxInstances = Just (toInteger section + 1)
              }
            } section seed
          matchInst `shouldSatisfy` isRight
