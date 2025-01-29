-- | Process additional test arguments

module Main where

import qualified Spec                             (spec)

import Settings                         (skipNeedsTuning)
import Test.Hspec.Core.Runner           (defaultConfig, hspecWith, readConfig)

import Data.IORef                       (atomicWriteIORef)
import System.Environment               (getArgs, withArgs)

data Tests
  = SkipNeedsTuning
  -- ^ excludes tests that fail sometimes
  -- or (might) take very long
  deriving Show

main :: IO ()
main = do
  (what, arguments) <- foldr separate ([], []) <$> getArgs
  case what of
    SkipNeedsTuning : _ -> atomicWriteIORef skipNeedsTuning True
    _ -> pure ()
  config <- readConfig defaultConfig arguments
  withArgs arguments $ hspecWith config Spec.spec
  where
    separate x (xs, ys) = case x of
      "--skip-needs-tuning" -> (SkipNeedsTuning : xs, ys)
      y -> (xs, y : ys)
