{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  OutputMonad (..),
  Rated,
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

import Control.Monad                    (foldM, unless, void, when)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (MonadWriter (pass, tell), Writer, execWriter,)
import Data.Containers.ListUtils        (nubOrd)
import Data.List                        (sort)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate (i)

hoveringInformation :: OutputMonad m => LangM m
hoveringInformation = do
  english [i|Please note: Hovering over or clicking on edges or their labels highlights both parts.|]
  german [i|Bitte beachten Sie: Beim Darüberbewegen oder Daraufklicken auf Kanten bzw. ihre Beschriftungen werden beide Teile hervorgehoben.|]

directionsAdvice :: OutputMonad m => LangM m
directionsAdvice = english [i|As navigation directions are used, please note that aggregations and compositions are only navigable from the "part" toward the "whole", i.e. they are not navigable in the opposite direction!|]

simplifiedInformation :: OutputMonad m => LangM m
simplifiedInformation = do
  english [i|Please note: Classes are represented simplified here.
That means they consist of a single box containing only its class name, but do not contain boxes for attributes and methods.
Nevertheless you should treat these simplified class representations as valid classes.|]
  german [i|Bitte beachten Sie: Klassen werden hier vereinfacht dargestellt.
Das heißt, sie bestehen aus einer einfachen Box, die nur den Klassennamenenthält, aber keine Abschnitte für Attribute oder Methoden.
Trotzdem sollten Sie diese vereinfachten Klassendarstellungen als valide Klassen ansehen.|]

yesNo :: OutputMonad m => Bool -> String -> LangM m
yesNo p q = do
  paragraph $ text q
  paragraph $ indent $
    if p
    then do
      english "Yes"
      german "Ja"
    else do
      english "No"
      german "Nein"

assertWith :: OutputMonad m => Rational -> Bool -> String -> LangM m
assertWith points = if points >= 1 % 2 then yesNo else assertion

multipleChoice
  :: (OutputMonad m, Ord a)
  => String
  -> Map a (Bool, b) -> [a]
  -> Rated m
multipleChoice what solution choices = do
  paragraph $ do
    english "Remarks on your solution:"
    german "Anmerkungen zur eingereichten Lösung:"
  let cs = sort $ nubOrd choices
      points = percentPer solution cs
  correct <- localise [
    (English, "Given " ++ what ++ " are correct?"),
    (German, "Die angegebenen " ++ what ++ " sind korrekt?")
    ]
  assertWith points (null [c | c <- cs, c `notElem` valid]) correct
  exhaustive <- localise [
    (English, "Given " ++ what ++ " are exhaustive?"),
    (German, "Die angegebenen " ++ what ++ " sind vollständig?")
    ]
  assertWith points (cs ==  valid) exhaustive
  return points
  where
    valid = M.keys $ M.filter ((== True) . fst) solution

singleChoice :: (OutputMonad m, Eq a) => String -> a -> a -> LangM m
singleChoice what solution choice = do
  paragraph (english "Remarks on your solution:")
  correct <- localise [(English, "Chosen " ++ what ++ " is correct?")]
  assertion (solution == choice) correct

percentPer :: Ord a => Map a (Bool, b) -> [a] -> Rational
percentPer xs = (% toInteger (length xs)) . sum
  . fmap (\y -> if maybe False fst $ M.lookup y xs then 1 else 0)

data Language = German | English
  deriving Eq

localise :: OutputMonad m => [(Language, String)] -> LangM' m String
localise lm = LangM $ \l ->
  return $ fromMaybe nonExistent $ lookup l lm
  where
    nonExistent
      | null lm   = error "missing translation"
      | otherwise = snd $ head lm

english :: OutputMonad m => String -> LangM m
english t = LangM $ \l -> when (l == English) $ withLang (text t) l

german :: OutputMonad m => String -> LangM m
german t = LangM $ \l -> when (l == German) $ withLang (text t) l

newtype LangM' m a = LangM { withLang :: Language -> m a}
type LangM m = LangM' m ()
type Rated m = LangM' m Rational

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

combineLangMs :: ([m a] -> m b) -> [LangM' m a] -> LangM' m b
combineLangMs f oms = LangM $ \l ->
  let rs = (`withLang` l) <$> oms
  in f rs

combineLangM :: (m a -> m b -> m c) -> LangM' m a -> LangM' m b -> LangM' m c
combineLangM f x y = LangM $ \l ->
  let x' = x `withLang` l
      y' = y `withLang` l
  in f x' y'

combineReports :: ([[o]] -> o) -> [LangM' (Report o) a] -> LangM' (Report o) ()
combineReports = combineLangMs . combineReports'

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
  -> [LangM' (Report o) a]
  -> LangM' (Report o) ()
combineWithReports f g = combineLangMs $ combineReports' (f . fmap g)

alignOutput
  :: ([o] -> o)
  -> LangM' (Report o) a
  -> LangM' (Report o) ()
alignOutput = mapLangM . alignOutput'

alignOutput'
  :: ([o] -> o)
  -> Report o a
  -> Report o ()
alignOutput' f r = Report $ do
  xs  <- MaybeT . return . sequence $ toOutput <$> getAllOuts r
  tell . (:[]) . Format $ f xs

toOutput :: Out o -> Maybe o
toOutput Abort      = Nothing
toOutput (Format x) = Just x

combineTwoReports
  :: ([o] -> [o] -> o)
  -> LangM' (Report o) a
  -> LangM' (Report o) b
  -> LangM' (Report o) ()
combineTwoReports = combineLangM . combineTwoReports'

combineTwoReports'
  :: ([o] -> [o] -> o)
  -> Report o a
  -> Report o b
  -> Report o ()
combineTwoReports' f r1 r2 = case sequence $ toOutput <$> getAllOuts r1 of
  Nothing -> void r1
  Just x  -> alignOutput' (f x) r2

format :: o -> LangM' (Report o) ()
format = lift . Report . tell . (:[]) . Format

abortWith :: o -> LangM' (Report o) b
abortWith d = do
  format d
  lift $ Report $ MaybeT (return Nothing)

toAbort :: LangM' (Report o) a -> LangM' (Report o) b
toAbort = mapLangM $ \r -> Report $ do
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
  itemizeM        = foldM ((>>) . return) ()
  indent xs       = xs
  latex _         = return ()
  code _          = return ()
