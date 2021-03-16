{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  OutputMonad (..),
  directionsAdvice,
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  Out (..),
  Report (..),
  abortWith,
  alignOutput,
  getAllOuts,
  combineReports,
  combineWithReports,
  combineTwoReports,
  format,
  toAbort,
  toOutput,
  ) where

import qualified Data.Map as M

import Data.String.Interpolate (i)
import Data.Map (Map)
import Data.List (nub, sort)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (MonadWriter (pass, tell), Writer, execWriter,)

hoveringInformation :: String
hoveringInformation = [i|Please note: hovering over or clicking on edges or their labels highlights both parts.|]

directionsAdvice :: String
directionsAdvice = [i|As navigation directions are used, please note that aggregations and composition are only navigable from the "part" toward the "whole", i.e they are not navigable in the opposite direction!|]

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
  refuse    :: m () -> m ()
  text      :: String -> m ()
  enumerateM :: (a -> m ()) -> [(a, m ())] -> m ()
  itemizeM   :: [m ()] -> m ()
  indent     :: m () -> m ()

data Out o = Format o | Abort

newtype Report o r = Report { unReport :: MaybeT (Writer [Out o]) r }
  deriving newtype (Applicative, Functor)

instance Monad (Report o) where
  return = Report . return
  Report r >>= f = Report $ r >>= unReport . f

getAllOuts :: Report o a -> [Out o]
getAllOuts r = execWriter $ do
  x <- runMaybeT $ unReport r
  case x of
    Nothing -> pass $ return ((), (Abort:))
    Just _ -> return ()

combineReports
  :: ([[o]] -> o)
  -> [Report o a]
  -> Report o ()
combineReports f rs = Report $ do
  os <- sequence $ toOut <$> rs
  tell . (:[]) . Format $ f os
  where
    toOut r = MaybeT . return . sequence $ toOutput <$> getAllOuts r

combineWithReports
  :: ([b] -> o)
  -> ([o] -> b)
  -> [Report o a]
  -> Report o ()
combineWithReports f g = combineReports (f . fmap g)

alignOutput
  :: ([o] -> o)
  -> Report o a
  -> Report o ()
alignOutput f r = Report $ do
  xs  <- MaybeT . return . sequence $ toOutput <$> getAllOuts r
  tell . (:[]) . Format $ f xs

toOutput :: Out o -> Maybe o
toOutput Abort      = Nothing
toOutput (Format x) = Just x

combineTwoReports
  :: ([o] -> [o] -> o)
  -> Report o a
  -> Report o b
  -> Report o ()
combineTwoReports f r1 r2 = case sequence $ toOutput <$> getAllOuts r1 of
  Nothing -> return ()
  Just x  -> alignOutput (f x) r2

format :: o -> Report o ()
format = Report . tell . (:[]) . Format

abortWith :: o -> Report o b
abortWith d = do
  format d
  Report $ MaybeT (return Nothing)

toAbort :: Report o a -> Report o b
toAbort r = Report $ do
  xs  <- MaybeT . return . sequence $ toOutput <$> getAllOuts r
  tell $ Format <$> xs
  MaybeT $ return Nothing
