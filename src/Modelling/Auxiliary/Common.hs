{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Modelling.Auxiliary.Common (
  ModellingTasksException (..),
  Object (..),
  Randomise (..),
  RandomiseLayout (..),
  RandomiseNames (..),
  ShuffleExcept (..),
  TaskGenerationException (..),
  findFittingRandomElements,
  getFirstInstance,
  lensRulesL,
  lowerFirst,
  mapIndicesTo,
  oneOf,
  parseInt,
  parseWith,
  skipSpaces,
  toMap,
  upperFirst,
  upperToDash,
  weightedShuffle,
  ) where

import qualified Data.Map                         as M (
  Map,
  empty,
  insertWith,
  )
import qualified Data.Set                         as S (
  Set,
  singleton,
  union,
  )

import Control.Exception                (Exception, SomeException)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.Extra              (firstJustM, ifM, maybeM)
import Control.Monad.Random (
  MonadRandom (getRandomR),
  RandT,
  fromList,
  )
import Control.Monad.Trans.Class        (lift)
import Data.Char (
  digitToInt,
  isSpace,
  isUpper,
  toLower,
  toUpper,
  )
import Data.Foldable                    (Foldable (foldl'))
import Data.Function                    ((&))
import Data.List                       (delete)
import Control.Lens (
  LensRules,
  (.~),
  lensField,
  lensRules,
  mappingNamer,
  )
import Math.Combinatorics.Exact.Binomial (choose)
import System.Random.Shuffle            (shuffleM)
import Text.Parsec                      (parse)
import Text.ParserCombinators.Parsec (
  Parser,
  digit,
  many,
  many1,
  optional,
  satisfy,
  )

data MatchListsException =
  FirstListIsLonger
  | SecondListIsLonger
  | ListsDoNotContainSameElements
  deriving Show

instance Exception MatchListsException

data ModellingTasksException
  = NeverHappens
  deriving Show

instance Exception ModellingTasksException

instance {-# OVERLAPPABLE #-} MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM

mapIndicesTo :: (Eq a, MonadThrow m) => [a] -> [a] -> m [(Int, Int)]
mapIndicesTo xs ys = mapIndicesToHelper (zip [0 ..] xs) (zip [0 ..] ys)

mapIndicesToHelper
  :: (Eq a, MonadThrow m)
  => [(Int, a)]
  -> [(Int, a)]
  -> m [(Int, Int)]
mapIndicesToHelper [] [] = pure []
mapIndicesToHelper [] _ = throwM SecondListIsLonger
mapIndicesToHelper _ [] = throwM FirstListIsLonger
mapIndicesToHelper ((k, x):xs) ys = do
  (l, ys') <- getFirstIn ys
  ((k, l) :) <$> mapIndicesToHelper xs ys'
  where
    getFirstIn [] = throwM ListsDoNotContainSameElements
    getFirstIn ((l, y) : ys')
      | x == y = pure (l, ys')
      | otherwise = fmap ((l, y) :) <$> getFirstIn ys'

newtype ShuffleExcept g a = ShuffleExcept {
  unShuffleExcept :: RandT g (Either SomeException) a
  }
  deriving (Applicative, Functor, Monad, MonadRandom)

instance MonadThrow (ShuffleExcept g) where
  throwM = ShuffleExcept . lift . throwM

{-|
The class of types that allow some form of randomisation.
-}
class Randomise a where
  -- | Shuffles every component without affecting basic overall properties
  randomise :: (MonadRandom m, MonadThrow m) => a -> m a

  -- | Checks the randomisability of the given value
  --     * returns Nothing, if it is randomisable
  --     * returns Just the explanation why not, otherwise
  isRandomisable :: a -> Maybe String
  isRandomisable _ = Nothing

{-|
The class of types that allow changing its layout randomly.
-}
class RandomiseLayout a where
  {-
  Shuffles the structure of every component
  without affecting its content and basic overall properties
  but by (maybe) affecting its layout.

  For a graph, for example, by changing the order of edges and nodes which affects
  how the used algorithm is laying out the graph.
  -}
  randomiseLayout :: (MonadRandom m, MonadThrow m) => a -> m a

{-|
The class of types that allow swapping (some of) its components names randomly.
-}
class RandomiseNames a where
  -- | Checks the randomisability of names for the given value
  --     * returns Nothing, if it is randomisable
  --     * returns Just the explanation why not, otherwise
  hasRandomisableNames :: a -> Maybe String
  hasRandomisableNames _ = Nothing

  -- | Shuffles the order of names of an instance, swapping names of components
  randomiseNames :: (MonadRandom m, MonadThrow m) => a -> m a

upperToDash :: String -> String
upperToDash [] = []
upperToDash (y:ys) = toLower y : foldr
  (\x xs -> if isUpper x then '-' : toLower x:xs else x:xs) ""
  ys

data Object = Object {
  oName :: String,
  oIndex :: Int
  } deriving (Eq, Ord, Show)

toMap :: (Ord a, Ord b) => S.Set (a, b) -> M.Map a (S.Set b)
toMap = foldr (\(x, y) -> M.insertWith S.union x (S.singleton y)) M.empty

oneOf :: MonadRandom m => [a] -> m a
oneOf xs = do
      x <- getRandomR (0, length xs - 1)
      return $ xs !! x

skipSpaces :: Parser ()
skipSpaces = optional $ many $ satisfy isSpace

parseInt :: Parser Int
parseInt = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (x:xs) = toLower x : xs

upperFirst :: String -> String
upperFirst []     = []
upperFirst (x:xs) = toUpper x : xs

lensRulesL :: LensRules
lensRulesL = lensRules & lensField .~ mappingNamer (pure . ('l':) . upperFirst)

parseWith :: MonadThrow m => (Int -> Parser a) -> String -> m a
parseWith f = either (throwM . ParsingException . show) pure . parse (f 0) ""

newtype ParsingException
  = ParsingException String
  deriving Show

instance Exception ParsingException

data TaskGenerationException =
  NoInstanceAvailable
  deriving Show

instance Exception TaskGenerationException

getFirstInstance :: MonadThrow m => [a] -> m a
getFirstInstance [] = throwM NoInstanceAvailable
getFirstInstance (x:_) = pure x

{-|
Provides a list of given elements with as many entries as provided predicates
by randomly picking given elements while ensuring as few repetitions
of these elements as possible occur.

Each predicate restricts an element in the resulting list (in order).
That means the resulting list is as long as the predicates list.
'Nothing' will be returned if there is no way to match all the predicates.

This function will attempt to distribute evenly, i.e. if 4 different elements
and 4 predicates are provided and no permutation fits,
'Nothing' will be returned although the predicates might hold for
e.g. choosing one of the elements 4 times.
-}
findFittingRandom
  :: MonadRandom m
  => [a]
  -- ^ elements to choose from
  -> [a -> m Bool]
  -- ^ predicates to satisfy
  -> m (Maybe [a])
findFittingRandom xs predicates = do
  xs' <- shuffleM $ concat
    $ replicate ((length predicates - 1) `div` length xs + 1) xs
  elementsFor predicates id xs'
  where
    elementsFor [] _ _ = pure (Just [])
    elementsFor _ _ [] = pure Nothing
    elementsFor (p : ps) prependFailed (c : cs) = do
      let retry = elementsFor (p : ps) (prependFailed . (c:)) cs
      ifM (p c)
        (maybeM retry (pure . Just . (c:)) $ elementsFor ps id $ prependFailed cs)
        retry

-- | Find fitting random elements with sophisticated distribution logic
-- Tries valid divisors in descending order with retry mechanism for each divisor
findFittingRandomElements
  :: MonadRandom m
  => Bool
  -- ^ useDifferentElements flag
  -> [a]
  -- ^ available elements
  -> [a -> m Bool]
  -- ^ predicates to satisfy
  -> m (Maybe [a])
findFittingRandomElements useDifferent availableElements predicates
  | useDifferent =
      let numAvailable = length availableElements
          numRequested = length predicates
          validNs = filter (\n -> numRequested `mod` n == 0) [numAvailable, numAvailable - 1 .. 2]
          tryDivisors [] = findFittingRandom availableElements predicates
          tryDivisors (n:ns) = tryDivisorWithRetries maxRetries
            where
                maxRetries = min 10 (2 * (numAvailable `choose` n) - 1)
                tryDivisorWithRetries 0 = tryDivisors ns  -- Exhausted retries, try next divisor
                tryDivisorWithRetries retries = do
                  selectedElements <- take n <$> shuffleM availableElements
                  result <- findFittingRandom selectedElements predicates
                  case result of
                    Nothing -> tryDivisorWithRetries (retries - 1)  -- Retry with different selection
                    Just elements -> pure (Just elements)
      in tryDivisors validNs
  | otherwise = do
      ds <- shuffleM availableElements
      firstJustM (\x -> findFittingRandom [x] predicates) ds

{-|
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
