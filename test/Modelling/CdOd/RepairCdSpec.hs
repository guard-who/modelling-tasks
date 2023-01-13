module Modelling.CdOd.RepairCdSpec where

import qualified Data.Map                         as M (null)

import Modelling.CdOd.RepairCd (
  RepairCdConfig (timeout),
  RepairCdInstance (changes),
  checkRepairCdConfig,
  classAndAssocNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  renameInstance,
  repairCd,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import System.Random.Shuffle            (shuffleM)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultRepairCdConfig" $
    it "is valid" $
      checkRepairCdConfig defaultRepairCdConfig `shouldBe` Nothing
  describe "repairCd" $
    context "using defaultRepairCdConfig with reduced timeouts" $
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . changes <$> repairCd cfg segment seed
        `shouldReturn` True
  describe "renameInstance" $
    it "is reversable" $ do
      let inst = defaultRepairCdInstance
          (names, assocs) = classAndAssocNames inst
      names' <- shuffleM names
      assocs' <- shuffleM assocs
      renamed <- renameInstance inst names' assocs'
      renameInstance renamed names assocs `shouldReturn` inst
  where
    cfg = defaultRepairCdConfig {
      timeout = Just 5000000
      }
