{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Type.hs
-}
module Modelling.PetriNet.Reach.Type where

import qualified Data.Map                         as M (
  filter,
  findWithDefault,
  fromList,
  lookup,
  mapKeys,
  toList,
  )
import qualified Data.Set                         as S (fromList, map)

import Modelling.Auxiliary.Common       (parseInt, skipSpaces)

import Control.Monad                    (void)
import Data.List                        (intercalate)
import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  char,
  optional,
  sepBy,
  skipMany,
  space,
  )

type Connection s t = ([s], t, [s])

newtype State s = State {unState :: Map s Int}
  deriving (Generic, Typeable)

mapState :: Ord b => (a -> b) -> State a -> State b
mapState f (State x) = State { unState = M.mapKeys f x }

instance Ord s => Eq (State s) where
  State f == State g = M.filter (/= 0) f == M.filter (/= 0) g

instance Ord s => Ord (State s) where
  compare (State f) (State g) =
    compare (M.filter (/= 0) f) (M.filter (/= 0) g)

instance Show s => Show (State s) where
  show = show . M.toList . unState

instance (Ord s, Read s) => Read (State s) where
  readsPrec p xs = do
    (s, ys) <- readsPrec p xs
    return (State . M.fromList $ s, ys)

mark :: Ord s => State s -> s -> Int
mark (State f) s = M.findWithDefault 0 s f

data Capacity s
  = Unbounded
  | AllBounded Int
  | Bounded (Map s Int)
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

mapCapacity :: Ord a => (s -> a) -> Capacity s -> Capacity a
mapCapacity _ Unbounded      = Unbounded
mapCapacity _ (AllBounded x) = AllBounded x
mapCapacity f (Bounded m)    = Bounded $ M.mapKeys f m

data Net s t = Net {
  places :: Set s,
  transitions :: Set t,
  connections :: [Connection s t],
  capacity :: Capacity s,
  start :: State s
  }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

bimapNet :: (Ord a, Ord b) => (s -> a) -> (t -> b) -> Net s t -> Net a b
bimapNet f g x = Net {
  places      = S.map f (places x),
  transitions = S.map g (transitions x),
  connections = map bimapConnection $ connections x,
  capacity    = mapCapacity f $ capacity x,
  start       = mapState f $ start x
  }
  where
    bimapConnection (w, y, z) = (f <$> w, g y, f <$> z)

allNonNegative :: State a -> Bool
allNonNegative (State z) =
  all (\(_, v) -> v >= 0) (M.toList z)

conforms :: Ord k => Capacity k -> State k -> Bool
conforms cap (State z) = case cap of
  Unbounded -> True
  AllBounded b ->
    all (\(_, v) -> v <= b) (M.toList z)
  Bounded f -> all
    (\(k, v) -> case M.lookup k f of
        Nothing -> True
        Just b -> v <= b
    )
    (M.toList z)

newtype Place = Place Int
  deriving (Enum, Eq, Generic, Ord, Read, Show, Typeable)

newtype ShowPlace = ShowPlace Place
  deriving (Eq, Ord)

instance Show ShowPlace where
  show (ShowPlace (Place p)) = "s" ++ show p

showPlace :: Place -> String
showPlace = show . ShowPlace

parsePlacePrec :: Int -> Parser Place
parsePlacePrec _ = do
  skipMany space
  void $ char 's'
  Place <$> parseInt <* skipMany space

newtype Transition = Transition Int
  deriving (Enum, Eq, Generic, Ord, Read, Show, Typeable)

newtype ShowTransition = ShowTransition Transition
  deriving (Eq, Ord)

instance Show ShowTransition where
  show (ShowTransition (Transition t)) = "t" ++ show t

showTransition :: Transition -> String
showTransition = show . ShowTransition

parseTransitionPrec :: Int -> Parser Transition
parseTransitionPrec _ = do
  skipMany space
  void $ char 't'
  Transition <$> parseInt <* skipMany space

newtype TransitionsList = TransitionsList {
  transitionsList :: [Transition]
  }
  deriving Generic

instance Show TransitionsList where
  show (TransitionsList ts) =
    '['
    : intercalate ", " (map showTransition ts)
    ++ "]"

parseTransitionsListPrec :: Int -> Parser TransitionsList
parseTransitionsListPrec _ = do
  skipSpaces
  optional $ char '['
  ts <- parseTransitionPrec 0 `sepBy` optional (char ',')
  skipSpaces
  optional $ char ']' >> skipSpaces
  return $ TransitionsList ts

example :: (Net Place Transition, State Place)
example =
  (Net {
    places = S.fromList [Place 1, Place 2, Place 3, Place 4],
    transitions = S.fromList [Transition 1, Transition 2, Transition 3, Transition 4],
    connections = [
        ([Place 3, Place 4], Transition 1, [Place 2]),
        ([Place 4], Transition 2, [Place 3]),
        ([Place 1], Transition 3, [Place 4]),
        ([Place 2], Transition 4, [Place 1])
    ],
    capacity = Unbounded,
    start = State $ M.fromList
      [(Place 1, 3), (Place 2, 0), (Place 3, 0), (Place 4, 0)]
    },
   State $ M.fromList [(Place 1, 0), (Place 2, 0), (Place 3, 1), (Place 4, 0)]
  )
