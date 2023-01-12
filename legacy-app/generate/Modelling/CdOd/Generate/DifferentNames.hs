{-# OPTIONS_GHC -Wwarn=deprecations #-}
module Modelling.CdOd.Generate.DifferentNames where

import Modelling.Auxiliary.Common       (Randomise (randomise))
import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance,
  getDifferentNamesTask,
  )
import Modelling.CdOd.Generate.Generate (generate)
import Modelling.CdOd.Types (
  ClassConfig (..),
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (MonadRandom, evalRandT, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

differentNames
  :: MonadIO m
  => DifferentNamesConfig
  -> Int
  -> Int
  -> ExceptT String m DifferentNamesInstance
differentNames config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  liftIO $ evalRandT fgen g
  where
    fgen = do
      configs <- withMinimalLabels 3 $ classConfig config
      inst <- continueWithHead configs $ \config' -> do
        (names, edges) <- generate
          (withNonTrivialInheritance config)
          config'
          (searchSpace config)
        getDifferentNamesTask fgen config names edges
      lift $ randomise inst
    continueWithHead []    _ = fgen
    continueWithHead (x:_) f = f x

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m [ClassConfig]
withMinimalLabels n config
  | n <= lowerLimit = return [config]
  | Just u <- upperLimit, n > u = return [config]
  | otherwise       = shuffleM
    [ config {
        aggregations = (aggrs, snd (aggregations config)),
        associations = (assos, snd (associations config)),
        compositions = (comps, snd (compositions config))
      }
    | aggrs <- range aggregations  0                           n
    , assos <- range associations  0                          (n - aggrs)
    , comps <- range compositions (max 0 $ n - aggrs - assos) (n - aggrs - assos)]
  where
    upperLimit = (\x y z -> x + y + z)
      <$> snd (aggregations config)
      <*> snd (associations config)
      <*> snd (compositions config)
    lowerLimit = 0
      + fst (aggregations config)
      + fst (associations config)
      + fst (compositions config)
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low + fst (f config) .. min' high (snd $ f config)]
