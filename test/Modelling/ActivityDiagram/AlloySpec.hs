module Modelling.ActivityDiagram.AlloySpec where

import Modelling.ActivityDiagram.Alloy (
  adConfigBitWidth,
  adConfigToAlloy',
  adConfigScope,
  )

import Modelling.ActivityDiagram.Config (
  defaultAdConfig,
  )

import Language.Alloy.Call              (getInstances)
import Test.Hspec                       (Spec, describe, it, context, shouldBe)


spec :: Spec
spec = do
  describe "adConfigScope" $
    context "given the default config" $ do
      let conf = defaultAdConfig
          bitWidth = adConfigBitWidth conf
      it "provides a scope large enough to generate instances" $ do
        let spec' = adConfigToAlloy' (adConfigScope conf) bitWidth "" "" conf
            depth = 10
        inst <- getInstances (Just depth) spec'
        length inst `shouldBe` fromIntegral depth
      it "provides a scope where half of it would not be sufficient to generate instances" $ do
        let spec' = adConfigToAlloy' (adConfigScope conf `div` 2) bitWidth "" "" conf
        inst <- getInstances (Just 1) spec'
        length inst `shouldBe` (0 :: Int)
  describe "adConfigBitWidth" $
    context "given the default config" $ do
      let conf = defaultAdConfig
          scope = adConfigScope conf
      it "provides a bit width large enough to generate instances" $ do
        let spec' = adConfigToAlloy' scope (adConfigBitWidth conf) "" "" conf
            depth = 10
        inst <- getInstances (Just depth) spec'
        length inst `shouldBe` fromIntegral depth
      it "provides a bit width where half of it would not be sufficient to generate instances" $ do
        let spec' = adConfigToAlloy' scope (adConfigBitWidth conf `div` 2) "" "" conf
        inst <- getInstances (Just 1) spec'
        length inst `shouldBe` (0 :: Int)
