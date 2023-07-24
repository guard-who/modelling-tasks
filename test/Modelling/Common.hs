{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Provides the ability to test code using the 'OutputMonad' by accepting success
and printing error messages.
-}
module Modelling.Common (
  withLang,
  withUnitTests,
  withUnitTestsUsingPath,
  ) where

import qualified Control.Monad.Output.Generic     as Output (withLang)

import Control.Monad                    (forM_, unless)
import Control.Monad.Output             (
  GenericLangM (unLangM),
  GenericOutputMonad (..),
  LangM',
  Language,
  )
import Control.Monad.Output.Generic (
  RunnableOutputMonad (..),
  )
import Control.Monad.Trans              (MonadTrans(lift))
import Data.List                        (isPrefixOf, sort)
import Data.List.Extra                  (replace)
import System.Directory                 (getDirectoryContents)
import System.FilePath                  ((</>), (-<.>))
import Test.Hspec

instance GenericOutputMonad Language (Either String) where
  assertion b m = unless b (m *> lift (Left "assertion"))
  image _         = pure ()
  images _ _ _    = pure ()
  paragraph _     = pure ()
  text _          = pure ()
  enumerateM _ _  = pure ()
  itemizeM _      = pure ()
  indent _        = pure ()
  refuse xs       = do
    xs
    lift $ Left "refused"
    pure ()
  latex _         = pure ()
  code _          = pure ()
  translated _    = pure ()
  translatedCode _ = pure ()

instance RunnableOutputMonad Language (Either String) where
  type Output Language (Either String) = Either String ()
  type RunMonad Language (Either String) = Either String
  runLangM = fmap ((, const $ Right ()) . Just) . unLangM

withLang
  :: LangM' (Either String) a
  -> Language
  -> Either String a
withLang x l = do
  res <- Output.withLang @Language @(Either String) x l
  maybe (Left "Aborted") Right res

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
    it (does ++ " for " ++ file) $ assertWith resultFile input expectedResult

withUnitTests
  :: String
  -> String
  -> FilePath
  -> String
  -> (String -> String -> Expectation)
  -> Spec
withUnitTests name does dir extension =
  withUnitTestsUsingPath name does dir extension . const
