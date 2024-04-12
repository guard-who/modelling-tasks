{-# LANGUAGE CPP #-}
-- | Central testing settings

module Settings where

configDepth :: Int
configDepth =
#if QUICK_TESTING
  4
#else
  5
#endif
