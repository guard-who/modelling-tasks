{-# LANGUAGE LambdaCase #-}
-- | Ignore additional test arguments

module Main where

import qualified Spec                             (spec)

import Test.Hspec.Core.Runner           (defaultConfig, hspecWith, readConfig)

import System.Environment               (getArgs, withArgs)

main :: IO ()
main = do
  arguments <- filter (not . ignore) <$> getArgs
  config <- readConfig defaultConfig arguments
  withArgs arguments $ hspecWith config Spec.spec
  where
    ignore = \case
      "--skip-needs-tuning" -> True
      _ -> False
