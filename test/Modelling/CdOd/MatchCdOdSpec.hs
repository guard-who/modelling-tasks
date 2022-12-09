module Modelling.CdOd.MatchCdOdSpec where

import Modelling.CdOd.MatchCdOd (
  checkMatchCdOdConfig,
  defaultMatchCdOdConfig,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "defaultMatchCdOdConfig" $
    it "is valid" $
      checkMatchCdOdConfig defaultMatchCdOdConfig `shouldBe` Nothing
