module Modelling.CdOd.RepairCdSpec where

import Modelling.CdOd.RepairCd (
  checkRepairCdConfig,
  defaultRepairCdConfig,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "defaultRepairCdConfig" $
    it "is valid" $
      checkRepairCdConfig defaultRepairCdConfig `shouldBe` Nothing
