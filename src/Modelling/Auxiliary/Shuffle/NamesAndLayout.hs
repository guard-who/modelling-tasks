{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Provides an interface to shuffle a 'taskInstance'
by providing a 'ShuffleInstance' datatype.

This module provides name and layout shuffling.
-}
module Modelling.Auxiliary.Shuffle.NamesAndLayout (
  ShuffleInstance (..),
  shuffleEverything,
  shuffleInstance,
  shuffleInstanceWith,
  ) where

import Modelling.Auxiliary.Common (
  RandomiseNames (randomiseNames),
  RandomiseLayout (randomiseLayout),
  ShuffleExcept (unShuffleExcept),
  )

import Control.Exception                (SomeException)
import Control.Monad                    ((>=>))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random             (MonadRandom, RandomGen, evalRandT)
import GHC.Generics                     (Generic)

-- | A datatype that allows setting name and layout shuffling
data ShuffleInstance a = ShuffleInstance {
  -- | The task instance to shuffle
  taskInstance :: !a,
  -- | If layout mangling should be permitted (affects graphics)
  allowLayoutMangling :: !Bool,
  -- | If names should be shuffled (affects component names)
  shuffleNames :: !Bool
  } deriving (Eq, Generic, Read, Show)

{-|
Set all shuffling methods to enabled.
-}
shuffleEverything
  :: (MonadRandom m, MonadThrow m, RandomiseLayout a, RandomiseNames a)
  => a
  -> m a
shuffleEverything inst = shuffleInstance ShuffleInstance {
  taskInstance = inst,
  allowLayoutMangling = True,
  shuffleNames = True
  }

{-|
Shuffle a 'taskInstance' based on enabled shuffling methods.
-}
shuffleInstance
  :: (MonadRandom m, MonadThrow m, RandomiseLayout a, RandomiseNames a)
  => ShuffleInstance a
  -> m a
shuffleInstance ShuffleInstance {..} =
  whenM shuffleNames randomiseNames
  >=> whenM allowLayoutMangling randomiseLayout
  $ taskInstance
  where
    whenM p x = if p then x else return

{-|
Shuffle a 'taskInstance' using 'shuffleInstance' and catch exceptions.
-}
shuffleInstanceWith
  :: (RandomGen g, RandomiseLayout a, RandomiseNames a)
  => ShuffleInstance a
  -> g
  -> Either SomeException a
shuffleInstanceWith x = evalRandT (unShuffleExcept $ shuffleInstance x)
