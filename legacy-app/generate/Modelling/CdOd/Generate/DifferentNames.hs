module Modelling.CdOd.Generate.DifferentNames where

import Capabilities.Alloy               (MonadAlloy)
import Modelling.Auxiliary.Shuffle.NamesAndLayout (
  shuffleEverything,
  )
import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance,
  getDifferentNamesTask,
  )
import Modelling.CdOd.Generate.Edges    (fromEdges)
import Modelling.CdOd.Generate.Generate (generate)
import Modelling.CdOd.Types (
  ClassConfig (..),
  )

import Control.Monad.Catch              (MonadCatch)
import Control.Monad.Random             (MonadRandom, evalRandT, mkStdGen)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

differentNames
  :: (MonadAlloy m, MonadCatch m)
  => Int
  -> DifferentNamesConfig
  -> Int
  -> Int
  -> m DifferentNamesInstance
differentNames searchSpace config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  evalRandT fgen g
  where
    fgen = do
      configs <- withMinimalLabels 3 $ classConfig config
      inst <- continueWithHead configs $ \config' -> do
        (names, edges) <- generate
          (withNonTrivialInheritance config)
          config'
          searchSpace
        getDifferentNamesTask fgen config $ fromEdges names edges
      shuffleEverything inst
    continueWithHead []    _ = fgen
    continueWithHead (x:_) f = f x

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m [ClassConfig]
withMinimalLabels n config
  | n <= lowerLimit = return [config]
  | Just u <- upperLimit, n > u = return [config]
  | otherwise       = shuffleM
    [ config {
        aggregationLimits = (aggrs, snd (aggregationLimits config)),
        associationLimits = (assos, snd (associationLimits config)),
        compositionLimits = (comps, snd (compositionLimits config))
      }
    | aggrs <- range aggregationLimits 0  n
    , assos <- range associationLimits 0 (n - aggrs)
    , comps <- range compositionLimits
        (max 0 $ n - aggrs - assos)
        (n - aggrs - assos)
    ]
  where
    upperLimit = (\x y z -> x + y + z)
      <$> snd (aggregationLimits config)
      <*> snd (associationLimits config)
      <*> snd (compositionLimits config)
    lowerLimit = 0
      + fst (aggregationLimits config)
      + fst (associationLimits config)
      + fst (compositionLimits config)
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low + fst (f config) .. min' high (snd $ f config)]
