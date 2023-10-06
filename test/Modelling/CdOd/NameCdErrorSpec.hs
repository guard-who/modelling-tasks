module Modelling.CdOd.NameCdErrorSpec where

import qualified Data.Map                         as M (null)

import Modelling.CdOd.NameCdError (
  NameCdErrorConfig (timeout),
  NameCdErrorInstance (allRelationships, errorReasons),
  checkNameCdErrorConfig,
  classAndAssocNames,
  defaultNameCdErrorConfig,
  defaultNameCdErrorInstance,
  renameInstance,
  nameCdErrorGenerate,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import System.Random.Shuffle            (shuffleM)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultNameCdErrorConfig" $
    it "is valid" $
      checkNameCdErrorConfig defaultNameCdErrorConfig `shouldBe` Nothing
  describe "nameCdErrorGenerate" $
    context "using defaultNameCdErrorConfig with reduced timeouts" $
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          let check x = not (M.null $ allRelationships x)
                && not (M.null $ errorReasons x)
          check <$> nameCdErrorGenerate cfg segment seed
        `shouldReturn` True
  describe "renameInstance" $
    it "is reversable" $ do
      let inst = defaultNameCdErrorInstance
          (names, assocs) = classAndAssocNames inst
      names' <- shuffleM names
      assocs' <- shuffleM assocs
      renamed <- renameInstance inst names' assocs'
      renameInstance renamed names assocs `shouldReturn` inst
  where
    cfg = defaultNameCdErrorConfig {
      timeout = Just 5000000
      }
