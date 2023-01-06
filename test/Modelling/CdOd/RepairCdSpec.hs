module Modelling.CdOd.RepairCdSpec where

import qualified Data.Map                         as M (null)

import Modelling.CdOd.RepairCd (
  RepairCdConfig (timeout),
  RepairCdInstance (changes),
  checkRepairCdConfig,
  defaultRepairCdConfig,
  repairCd,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultRepairCdConfig" $
    it "is valid" $
      checkRepairCdConfig defaultRepairCdConfig `shouldBe` Nothing
  describe "repairCd" $
    context "using defaultRepairCdConfig with reduced timeouts" $
      it "generates an instance" $ do
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . changes <$> repairCd cfg segment seed
        `shouldReturn` True
  where
    cfg = defaultRepairCdConfig {
      timeout = Just 5000000
      }
