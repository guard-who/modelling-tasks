module Modelling.CdOd.SelectValidCdSpec where

import qualified Data.Map                         as M (null)

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.SelectValidCd (
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
    context "using defaultSelectValidCdConfig" $
      it "generates an instance" $ do
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . classDiagrams
            <$> selectValidCd defaultSelectValidCdConfig segment seed
        `shouldReturn` True
