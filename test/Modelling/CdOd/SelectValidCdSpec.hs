module Modelling.CdOd.SelectValidCdSpec where

import qualified Data.Map                         as M (null)

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (timeout),
  SelectValidCdInstance (classDiagrams),
  checkSelectValidCdConfig,
  defaultSelectValidCdConfig,
  selectValidCd,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultSelectValidCdConfig" $
    it "is valid" $
      checkSelectValidCdConfig defaultSelectValidCdConfig `shouldBe` Nothing
  describe "selectValidCd" $
    context "using defaultSelectValidCdConfig with reduced timeouts" $
      it "generates an instance" $ do
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . classDiagrams <$> selectValidCd cfg segment seed
        `shouldReturn` True
  where
    cfg = defaultSelectValidCdConfig {
      timeout = Just 5000000
      }
