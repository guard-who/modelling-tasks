{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  OutputMonad (..),
  directionsAdvice,
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  ) where

import qualified Data.Map as M

import Data.String.Interpolate (i)
import Data.Map (Map)
import Data.List (nub, sort)

hoveringInformation :: String
hoveringInformation = [i|Please note: hovering over or clicking on edges or their labels highlights both parts.|]

directionsAdvice :: String
directionsAdvice = [i|As navigation directions are used, please note that aggregations and composition are navigable from the "part" toward the "whole".|]

simplifiedInformation :: String
simplifiedInformation = [i|Please note: classes are represented simplified here.
That means they consist of a single box containing only its class name, but do not contain boxes for attributes and methods.
Nevertheless you should treat these simplified class representations as valid classes.|]

multipleChoice :: (OutputMonad m, Ord a) => String -> Map a (Bool, b) -> [a] -> m ()
multipleChoice what solution choices = do
  paragraph $ text "Remarks on your solution:"
  let cs = nub $ sort choices
  assertion (null [c | c <- cs, c `notElem` valid])
    $ "Given " ++ what ++ " are correct?"
  assertion (cs ==  valid) $ "Given " ++ what ++ " are exhaustive?"
  where
    valid = M.keys $ M.filter ((== True) . fst) solution

class Monad m => OutputMonad m where
  assertion :: Bool -> String -> m ()
  enumerate :: (k -> String) -> (a -> String) -> Map k a -> m ()
  image     :: FilePath -> m ()
  images    :: (k -> String) -> (a -> FilePath) -> Map k a -> m ()
  paragraph :: m () -> m ()
  text      :: String -> m ()
  enumerateM :: (a -> m ()) -> [(a, m ())] -> m ()
  itemizeM   :: [m ()] -> m ()
