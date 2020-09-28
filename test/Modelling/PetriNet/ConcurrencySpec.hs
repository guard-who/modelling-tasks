module Modelling.PetriNet.ConcurrencySpec where

import Modelling.PetriNet.Concurrency
import Modelling.PetriNet.Types
  (defaultFindConcurrencyConfig,defaultPickConcurrencyConfig)

import Control.Monad.Trans.Except       (runExceptT)
import Test.Hspec

spec :: Spec
spec = do
  describe "findConcurrency" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConc <- runExceptT $ findConcurrency 0 defaultFindConcurrencyConfig
        print (snd diaConc) `shouldReturn` ()
  describe "pickConcurrency" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConc <- runExceptT $ pickConcurrency 0 defaultPickConcurrencyConfig
        print (map snd diaConc) `shouldReturn` ()
