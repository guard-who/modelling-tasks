{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Alloy

module Capabilities.Alloy.IO () where

import Capabilities.Alloy               (MonadAlloy (..))

import qualified Language.Alloy.Call              as Alloy (
  getInstancesWith,
  )

instance MonadAlloy IO where
  getInstancesWith = Alloy.getInstancesWith

