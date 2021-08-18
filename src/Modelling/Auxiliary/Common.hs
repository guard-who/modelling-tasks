module Modelling.Auxiliary.Common where

import Control.Monad.Random (MonadRandom (getRandomR))

oneOf :: MonadRandom m => [a] -> m a
oneOf xs = do
      x <- getRandomR (0, length xs - 1)
      return $ xs !! x
