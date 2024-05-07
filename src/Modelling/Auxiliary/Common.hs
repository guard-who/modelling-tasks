{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Modelling.Auxiliary.Common (
  Object (..),
  Randomise (..),
  RandomiseLayout (..),
  ShuffleInstance (..),
  TaskGenerationException (..),
  getFirstInstance,
  lensRulesL,
  lowerFirst,
  mapIndicesTo,
  oneOf,
  parseInt,
  parseWith,
  shuffleEverything,
  shuffleInstanceWith,
  skipSpaces,
  toMap,
  upperFirst,
  upperToDash,
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
import Control.Monad                    ((>=>))
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.Random (
  MonadRandom (getRandomR),
  RandomGen,
  RandT,
  evalRandT,
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
import Control.Lens (
  LensRules,
  (.~),
  lensField,
  lensRules,
  mappingNamer,
  )
import GHC.Generics                     (Generic)
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

instance MonadThrow m => MonadThrow (RandT g m) where
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

shuffleInstanceWith
  :: (RandomGen g, Randomise a, RandomiseLayout a)
  => ShuffleInstance a
  -> g
  -> Either SomeException a
shuffleInstanceWith x = evalRandT (unShuffleExcept $ shuffleInstance x)

shuffleEverything
  :: (MonadRandom m, MonadThrow m, Randomise a, RandomiseLayout a)
  => a
  -> m a
shuffleEverything inst = shuffleInstance $ ShuffleInstance {
  taskInstance                = inst,
  allowLayoutMangling         = True,
  shuffleNames                = True
  }

data ShuffleInstance a = ShuffleInstance {
  taskInstance                :: a,
  allowLayoutMangling         :: Bool,
  shuffleNames                :: Bool
  } deriving (Eq, Generic, Read, Show)

shuffleInstance
  :: (MonadRandom m, MonadThrow m, Randomise a, RandomiseLayout a)
  => ShuffleInstance a
  -> m a
shuffleInstance ShuffleInstance {..} =
  whenM shuffleNames randomise
  >=> whenM allowLayoutMangling randomiseLayout
  $ taskInstance
  where
    whenM p x = if p then x else return

class Randomise a where
  -- | Shuffles every component without affecting basic overall properties
  randomise :: (MonadRandom m, MonadThrow m) => a -> m a

  -- | Checks the randomisability of the given value
  --     * returns Nothing, if it is randomisable
  --     * returns Just the explanation why not, otherwise
  isRandomisable :: a -> Maybe String
  isRandomisable _ = Nothing

class RandomiseLayout a where
  {-
  Shuffles the structure of every component
  without affecting its content and basic overall properties
  but by (maybe) affecting its layout.

  For a graph, for example, by changing the order of edges and nodes which affects
  how the used algorithm is laying out the graph.
  -}
  randomiseLayout :: (MonadRandom m, MonadThrow m) => a -> m a

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
