{-# LANGUAGE CPP #-}
-- | Central testing settings

module Settings where

configDepth :: Int
configDepth =
#if QUICK_TESTING
  6
#else
  4
#endif
