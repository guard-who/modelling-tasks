{-# LANGUAGE FlexibleInstances #-}
{- |
Provides the ability to test code using the 'OutputMonad' by accepting success
and printing error messages.
-}
module Modelling.Common where


import Control.Monad                    (unless)
import Control.Monad.Output             (OutputMonad (..))
import Control.Monad.Trans              (MonadTrans(lift))

instance OutputMonad (Either String) where
  assertion b m = unless b (m >> lift (Left "assertion"))
  image _         = return ()
  images _ _ _    = return ()
  paragraph _     = return ()
  text _          = return ()
  enumerateM _ _  = return ()
  itemizeM _      = return ()
  indent _        = return ()
  refuse xs       = do
    xs
    lift $ Left "refused"
  latex _         = return ()
  code _          = return ()
  translated _    = return ()
