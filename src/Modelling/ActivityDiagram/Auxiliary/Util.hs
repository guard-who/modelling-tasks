module Modelling.ActivityDiagram.Auxiliary.Util (
  failWith,
  headEither,
  headWithErr,
  weightedShuffle
  ) where

import Control.Monad.Random (
  StdGen,
  fromList,
  runRand
  )
import Data.List (delete)
import Data.Maybe (listToMaybe)

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

headEither :: a -> [c] -> Either a c
headEither err xs =
  case listToMaybe xs of
    Nothing -> Left err
    Just x -> Right x

headWithErr :: String -> [a] -> a
headWithErr err = failWith id . headEither err

{-
  Shuffle a list of elements from type a based on given weights of type w,
  where higher weight indicates a bigger probability of the element occuring
  at a lower index of the list. The total weight of all elements must not be zero.
-}
weightedShuffle
  :: (Eq a, Real w)
  => StdGen
  -> [(a,w)]
  -> [a]
weightedShuffle _ [] = []
weightedShuffle g xs =
  let rs = map (\x -> (x, toRational $ snd x)) xs
      (a, g') = runRand (fromList rs) g
  in fst a : weightedShuffle g' (delete a xs)