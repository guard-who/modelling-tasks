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
  toList,
  )
import qualified Data.Set                         as S (fromList)

import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

type Connection s t = ([s], t, [s])

newtype State s = State {unState :: Map s Int}
  deriving (Typeable, Generic)

instance Ord s => Eq (State s) where
  State f == State g = M.filter (/= 0) f == M.filter (/= 0) g

instance Ord s => Ord (State s) where
  compare (State f) (State g) =
    compare (M.filter (/= 0) f) (M.filter (/= 0) g)

instance Show s => Show (State s) where
  show = show . M.toList . unState

mark :: Ord s => State s -> s -> Int
mark (State f) s = M.findWithDefault 0 s f

data Capacity s
  = Unbounded
  | AllBounded Int
  | Bounded (Map s Int)
  deriving (Eq, Generic, Ord, Show, Typeable)

data Net s t = Net {
  places :: Set s,
  transitions :: Set t,
  connections :: [Connection s t],
  capacity :: Capacity s,
  start :: State s
  }
  deriving (Eq, Generic, Ord, Show, Typeable)

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
  deriving (Eq, Ord, Typeable, Generic, Enum)

instance Show Place where
  show (Place p) = "s" ++ show p

newtype Transition = Transition Int
  deriving (Eq, Ord, Typeable, Generic, Enum)

instance Show Transition where
  show (Transition t) = "t" ++ show t

example :: (Net Place Transition, State Place)
example =
  (Net {
    places = S.fromList [Place 1, Place 2, Place 3, Place 4],
    transitions = S.fromList [Transition 1, Transition 2, Transition 3, Transition 4],
    connections = [
        ([Place 3], Transition 1, [Place 2, Place 3]),
        ([Place 4], Transition 2, [Place 3]),
        ([Place 1], Transition 3, [Place 4]),
        ([Place 2], Transition 4, [Place 1])
    ],
    capacity = AllBounded 1,
    start = State $ M.fromList
      [(Place 1, 1), (Place 2, 0), (Place 3, 0), (Place 4, 0)]
    },
   State $ M.fromList [(Place 1, 1), (Place 2, 1), (Place 3, 1), (Place 4, 1)]
  )
