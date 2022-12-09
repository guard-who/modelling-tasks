module Modelling.CdOd.SelectValidCdSpec where

import Modelling.CdOd.SelectValidCd (
  checkSelectValidCdConfig,
  defaultSelectValidCdConfig,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "defaultSelectValidCdConfig" $
    it "is valid" $
      checkSelectValidCdConfig defaultSelectValidCdConfig `shouldBe` Nothing
