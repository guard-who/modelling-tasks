{-# LANGUAGE CPP #-}
-- | Defines a Monad context for calling Alloy

module Capabilities.Alloy (
  MonadAlloy (..),
  getInstances,
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Language.Alloy.Call (
  AlloyInstance,
  CallAlloyConfig (..),
  SatSolver (..),
  defaultCallAlloyConfig,
  )
import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Trans.Random       (RandT)
import Control.OutputCapable.Blocks.Generic (GenericReportT)

class Monad m => MonadAlloy m where
  getInstancesWith :: CallAlloyConfig -> String -> m [AlloyInstance]

instance MonadAlloy m => MonadAlloy (RandT g m) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadAlloy m => MonadAlloy (ExceptT e m) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadAlloy m => MonadAlloy (GenericReportT l o m)  where
  getInstancesWith config = lift . getInstancesWith config

getInstances
  :: MonadAlloy m
  => Maybe Integer
  -> Maybe Int
  -> String
  -> m [AlloyInstance]
getInstances maybeMaxInstances maybeTimeout = getInstancesWith
  $ defaultCallAlloyConfig {
    maxInstances = maybeMaxInstances,
#if ALLOY_USE_SAT4J
    satSolver    = SAT4J,
#else
    satSolver    = MiniSat,
#endif
    timeout      = maybeTimeout
    }
