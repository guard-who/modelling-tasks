{-# LANGUAGE FlexibleInstances #-}
{- |
Provides the ability to test code using the 'OutputMonad' by accepting success
and printing error messages.
-}
module Modelling.Common (
  withUnitTests,
  withUnitTestsUsingPath,
  ) where


import Control.Monad                    (forM_, unless)
import Control.Monad.Output             (OutputMonad (..))
import Control.Monad.Trans              (MonadTrans(lift))
import Data.List                        (isPrefixOf, sort)
import Data.List.Extra                  (replace)
import System.Directory                 (getDirectoryContents)
import System.FilePath                  ((</>), (-<.>))
import Test.Hspec

instance OutputMonad (Either String) where
  assertion b m = unless b (m >> lift (Left "assertion"))
  image _         = return ()
  images _ _ _    = return ()
  paragraph _     = return ()
  text _          = return ()
  enumerateM _ _  = return ()
  itemizeM _      = return ()
  indent _        = return ()
  refuse xs       = do
    xs
    lift $ Left "refused"
  latex _         = return ()
  code _          = return ()
  translated _    = return ()

withUnitTestsUsingPath
  :: String
  -> String
  -> FilePath
  -> String
  -> (FilePath -> String -> String -> Expectation)
  -> Spec
withUnitTestsUsingPath name does dir extension assertWith = describe name $ do
  fs <- runIO $ sort <$> getDirectoryContents dir
  let testName = name ++ "Test"
  forM_ (filter (testName `isPrefixOf`) fs) $ \fileName -> do
    let file = dir </> fileName
    input <- runIO $ readFile file
    let resultFile = replace "Test" "Result" file -<.> extension
    expectedResult <- runIO $ readFile resultFile
    it (does ++ " for " ++ file) $ assertWith file input expectedResult

withUnitTests
  :: String
  -> String
  -> FilePath
  -> String
  -> (String -> String -> Expectation)
  -> Spec
withUnitTests name does dir extension =
  withUnitTestsUsingPath name does dir extension . const
