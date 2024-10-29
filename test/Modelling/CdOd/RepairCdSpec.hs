module Modelling.CdOd.RepairCdSpec where

import qualified Data.Map                         as M (null)

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.RepairCd (
  RepairCdInstance (changes),
  checkRepairCdConfig,
  checkRepairCdInstance,
  classAndNonInheritanceNames,
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
  describe "defaultRepairCdInstance" $
    it "is valid" $
      checkRepairCdInstance defaultRepairCdInstance `shouldBe` Nothing
  describe "repairCd" $
    context "using defaultRepairCdConfig with limited instances" $ do
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . changes <$> repairCd defaultRepairCdConfig segment seed
        `shouldReturn` True
      it "reproducible generates defaultRepairCdInstance" $
        repairCd defaultRepairCdConfig 0 0
        `shouldReturn` defaultRepairCdInstance
  describe "renameInstance" $
    it "is reversable" $ do
      let inst = defaultRepairCdInstance
          (names, nonInheritances) = classAndNonInheritanceNames inst
      names' <- shuffleM names
      nonInheritances' <- shuffleM nonInheritances
      renamed <- renameInstance inst names' nonInheritances'
      renameInstance renamed names nonInheritances `shouldReturn` inst
