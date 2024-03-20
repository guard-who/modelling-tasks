-- | Defines a Monad context for calling Alloy

module Capabilities.Alloy (
  MonadAlloy (..),
  getInstances,
  )where

import qualified Language.Alloy.Call              as Alloy (
  getInstancesWith,
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output.Generic     (GenericReportT)
import Language.Alloy.Call (
  AlloyInstance,
  CallAlloyConfig (..),
  SatSolver (..),
  defaultCallAlloyConfig,
  )

class Monad m => MonadAlloy m where
  getInstancesWith :: CallAlloyConfig -> String -> m [AlloyInstance]

instance MonadAlloy IO where
  getInstancesWith = Alloy.getInstancesWith

instance MonadIO m => MonadAlloy (GenericReportT l o m)  where
  getInstancesWith config = liftIO . getInstancesWith config

getInstances
  :: MonadAlloy m
  => Maybe Integer
  -> Maybe Int
  -> String
  -> m [AlloyInstance]
getInstances maybeMaxInstances maybeTimeout = getInstancesWith
  $ defaultCallAlloyConfig {
    maxInstances = maybeMaxInstances,
    satSolver    = MiniSat,
    timeout      = maybeTimeout
    }
