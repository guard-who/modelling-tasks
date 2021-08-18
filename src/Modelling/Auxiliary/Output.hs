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
  singleChoice,
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
  LangM,
  LangM' (LangM),
  Language (..),
  english,
  german,
  localise,
  mapLangM,
  withLang,
  ) where

import qualified Data.Map as M

import Control.Monad                    (foldM, unless, when)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (MonadWriter (pass, tell), Writer, execWriter,)
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)

hoveringInformation :: OutputMonad m => LangM m
hoveringInformation = english [i|Please note: Hovering over or clicking on edges or their labels highlights both parts.|]

directionsAdvice :: OutputMonad m => LangM m
directionsAdvice = english [i|As navigation directions are used, please note that aggregations and compositions are only navigable from the "part" toward the "whole", i.e. they are not navigable in the opposite direction!|]

simplifiedInformation :: OutputMonad m => LangM m
simplifiedInformation = english [i|Please note: Classes are represented simplified here.
That means they consist of a single box containing only its class name, but do not contain boxes for attributes and methods.
Nevertheless you should treat these simplified class representations as valid classes.|]

multipleChoice
  :: (OutputMonad m, Ord a)
  => String
  -> Map a (Bool, b) -> [a]
  -> LangM m
multipleChoice what solution choices = do
  paragraph (english "Remarks on your solution:")
  let cs = nub $ sort choices
  correct <- localise [(English, "Given " ++ what ++ " are correct?")]
  assertion (null [c | c <- cs, c `notElem` valid]) correct
  exhaustive <- localise [(English, "Given " ++ what ++ " are exhaustive?")]
  assertion (cs ==  valid) exhaustive
  where
    valid = M.keys $ M.filter ((== True) . fst) solution

singleChoice :: (OutputMonad m, Eq a) => String -> a -> a -> LangM m
singleChoice what solution choice = do
  paragraph (english "Remarks on your solution:")
  correct <- localise [(English, "Chosen " ++ what ++ " is correct?")]
  assertion (solution == choice) correct

data Language = German | English
  deriving Eq

localise :: OutputMonad m => [(Language, String)] -> LangM' m String
localise lm = LangM $ \l ->
  return $ fromMaybe (error "missing translation") $ lookup l lm

english :: OutputMonad m => String -> LangM m
english t = LangM $ \l -> when (l == English) $ withLang (text t) l

german :: OutputMonad m => String -> LangM m
german t = LangM $ \l -> when (l == German) $ withLang (text t) l

newtype LangM' m a = LangM { withLang :: Language -> m a}
type LangM m = LangM' m ()

instance Functor m => Functor (LangM' m) where
  fmap f (LangM o) = LangM $ fmap f . o

instance Applicative m => Applicative (LangM' m) where
  pure x = LangM . const $ pure x
  LangM f <*> LangM x = LangM $ \l -> f l <*> x l

instance Monad m => Monad (LangM' m) where
  LangM x >>= f = LangM $ \l -> x l >>= (\x' -> withLang (f x') l)

instance MonadTrans LangM' where
  lift m = LangM $ const m

class Monad m => OutputMonad m where
  assertion :: Bool -> String -> LangM m
  enumerate :: (k -> String) -> (a -> String) -> Map k a -> LangM m
  image     :: FilePath -> LangM m
  images    :: (k -> String) -> (a -> FilePath) -> Map k a -> LangM m
  paragraph :: LangM m -> LangM m
  refuse    :: LangM m -> LangM m
  text      :: String -> LangM m
  enumerateM :: (a -> LangM m) -> [(a, LangM m)] -> LangM m
  itemizeM   :: [LangM m] -> LangM m
  indent     :: LangM m -> LangM m
  latex      :: String -> LangM m
  code       :: String -> LangM m

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

combineReports :: ([[o]] -> o) -> [LangM' (Report o) a] -> LangM' (Report o) ()
combineReports f oms = LangM $ \l ->
  let rs = (`withLang` l) <$> oms
  in combineReports' f rs

combineReports'
  :: ([[o]] -> o)
  -> [Report o a]
  -> Report o ()
combineReports' f rs = Report $ do
  os <- sequence $ toOut <$> rs
  tell . (:[]) . Format $ f os
  where
    toOut r = MaybeT . return . sequence $ toOutput <$> getAllOuts r

combineWithReports
  :: ([b] -> o)
  -> ([o] -> b)
  -> [Report o a]
  -> Report o ()
combineWithReports f g = combineReports' (f . fmap g)

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

mapLangM :: (m a -> m b) -> LangM' m a -> LangM' m b
mapLangM f om = LangM $ f . withLang om

instance OutputMonad Maybe where
  assertion b _   = unless b $ lift Nothing
  enumerate _ _ _ = return ()
  image _         = return ()
  images _ _ _    = return ()
  paragraph xs    = xs
  refuse xs       = xs >> lift Nothing
  text _          = return ()
  enumerateM f xs = (\(x, y) -> f x >> y) `mapM_` xs
  itemizeM xs     = foldM ((>>) . return) () xs
  indent xs       = xs
  latex _         = return ()
  code _          = return ()
