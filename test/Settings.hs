{-# LANGUAGE CPP #-}
-- | Central testing settings

module Settings where

import Data.IORef                       (IORef, newIORef, readIORef)
import System.IO.Unsafe                 (unsafePerformIO)
import Test.Hspec                       (Spec, describe, runIO, xdescribe)

{-# NOINLINE skipNeedsTuning #-}
skipNeedsTuning :: IORef Bool
skipNeedsTuning = unsafePerformIO (newIORef False)

needsTuning :: Spec -> Spec
needsTuning spec = do
  skip <- runIO $ readIORef skipNeedsTuning
  if skip
    then xdescribe "needs tuning (skipping)" spec
    else describe "needs tuning" spec

configDepth :: Int
configDepth =
#if QUICK_TESTING
  4
#else
  5
#endif
