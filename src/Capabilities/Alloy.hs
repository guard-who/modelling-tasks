-- | Defines a Monad context for calling Alloy

module Capabilities.Alloy (
  MonadAlloy (..),
  getInstances,
  )where

import qualified Language.Alloy.Call              as Alloy (
  getInstancesWith,
  )

import Language.Alloy.Call              as Alloy (
  AlloyInstance,
  CallAlloyConfig (..),
  SatSolver (..),
  defaultCallAlloyConfig,
  )

class Monad m => MonadAlloy m where
  getInstancesWith :: CallAlloyConfig -> String -> m [AlloyInstance]

instance MonadAlloy IO where
  getInstancesWith = Alloy.getInstancesWith

getInstances
  :: MonadAlloy m
  => Maybe Integer
  -> Maybe Int
  -> String
  -> m [AlloyInstance]
getInstances mmaxInstances mtimeout = getInstancesWith $ defaultCallAlloyConfig {
  maxInstances = mmaxInstances,
  satSolver    = MiniSat,
  timeout      = mtimeout
  }
