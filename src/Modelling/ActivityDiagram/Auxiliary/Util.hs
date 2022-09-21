module Modelling.ActivityDiagram.Auxiliary.Util (
  failWith,
  headEither,
  headWithErr
  ) where

import Data.Maybe (listToMaybe)

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

headEither :: a -> [c] -> Either a c
headEither err xs =
  case listToMaybe xs of
    Nothing -> Left err
    Just x -> Right x

headWithErr :: String -> [a] -> a
headWithErr err = (failWith id) . headEither err