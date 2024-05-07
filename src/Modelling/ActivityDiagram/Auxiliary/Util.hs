module Modelling.ActivityDiagram.Auxiliary.Util (
  weightedShuffle
  ) where

import Control.Monad.Random (
  MonadRandom,
  fromList,
  )
import Data.List (delete)

{-
  Shuffle a list of elements from type a based on given weights of type w,
  where higher weight indicates a bigger probability of the element occurring
  at a lower index of the list. The total weight of all elements must not be zero.
-}
weightedShuffle
  :: (MonadRandom m, Eq a, Real w)
  => [(a,w)]
  -> m [a]
weightedShuffle [] = return []
weightedShuffle xs = do
  let rs = map (\x -> (x, toRational $ snd x)) xs
  a <- fromList rs
  ys <- weightedShuffle (delete a xs)
  return (fst a : ys)
