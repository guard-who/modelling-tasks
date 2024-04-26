{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Alloy

module Capabilities.Alloy.IO () where

import Capabilities.Alloy               (MonadAlloy (..))

import qualified Language.Alloy.Call              as Alloy (
  getInstancesWith,
  )
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output.Generic     (GenericReportT)

instance MonadAlloy IO where
  getInstancesWith = Alloy.getInstancesWith

instance MonadIO m => MonadAlloy (GenericReportT l o m)  where
  getInstancesWith config = liftIO . getInstancesWith config
