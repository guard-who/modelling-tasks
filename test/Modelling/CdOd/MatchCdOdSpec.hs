module Modelling.CdOd.MatchCdOdSpec where

import qualified Data.Map                         as M (null)

import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (timeout),
  checkMatchCdOdConfig,
  defaultMatchCdOdConfig,
  diagrams,
  matchCdOd,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultMatchCdOdConfig" $
    it "is valid" $
      checkMatchCdOdConfig defaultMatchCdOdConfig `shouldBe` Nothing
  describe "matchCdOd" $
    context "using defaultMatchCdOdConfig with reduced timeouts" $
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . diagrams <$> matchCdOd cfg segment seed
        `shouldReturn` True
  where
    cfg = defaultMatchCdOdConfig {
      timeout = Just 5000000
      }
