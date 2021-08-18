{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Deadlock.hs
-}
module Modelling.PetriNet.Reach.Deadlock where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

import Modelling.Auxiliary.Common       (oneOf)
import Modelling.Auxiliary.Output (
  LangM,
  OutputMonad (assertion),
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Reach   (isNoLonger, reportReachFor)
import Modelling.PetriNet.Reach.Roll    (net)
import Modelling.PetriNet.Reach.Step    (deadlocks, executes, successors)
import Modelling.PetriNet.Reach.Type (
  Transition (..),
  Place (..),
  Net (..),
  Capacity (Unbounded),
  State (State),
  )

import Control.Monad                    (forM, guard)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Random             (MonadRandom, evalRand, mkStdGen)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (maximumBy)
import Data.Ord                         (comparing)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

data PetriDeadlock = PetriDeadlock
  deriving (Typeable, Generic)

verifyDeadlock
  :: (OutputMonad m, Show a, Show t, Ord t, Ord a)
  => PetriDeadlock
  -> Net a t
  -> LangM m
verifyDeadlock PetriDeadlock = validate Default

reportDeadlock
  :: (OutputMonad m, MonadIO m, Ord s, Ord t, Show s, Show t)
  => FilePath
  -> DeadlockInstance s t
  -> LangM m
reportDeadlock path inst = do
  img <- drawToFile True path (drawUsing inst) 0 $ petriNet inst
  reportReachFor
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (withMinLengthHint inst)
    Nothing

initialDeadlock :: DeadlockInstance s a -> [a]
initialDeadlock inst = reverse $ S.toList $ transitions $ petriNet inst

totalDeadlock
  :: (OutputMonad m, MonadIO m, Show t, Show s, Ord t, Ord s)
  => FilePath
  -> DeadlockInstance s t
  -> [t]
  -> LangM m
totalDeadlock path inst ts = do
  isNoLonger (noLongerThan inst) ts
  out <- executes path True (drawUsing inst) n ts
  assertion (null $ successors n out) "Zielmarkierung hat keine Nachfolger?"
  where
    n = petriNet inst

data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Maybe Int
  } deriving (Typeable, Generic)

data Config = Config {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool
  }
  deriving (Typeable, Generic)

defaultDeadlockConfig :: Config
defaultDeadlockConfig =
  Config {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Deadlock.capacity = Unbounded,
  drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
  maxTransitionLength = 10,
  minTransitionLength = 8,
  rejectLongerThan    = Nothing,
  showLengthHint      = True,
  showMinLengthHint   = True
  }

generateDeadlock :: Config -> Int -> DeadlockInstance Place Transition
generateDeadlock conf seed = DeadlockInstance {
  drawUsing         = cmd,
  noLongerThan      = rejectLongerThan conf,
  petriNet          = petri,
  withLengthHint    =
    if showLengthHint conf then Just $ maxTransitionLength conf else Nothing,
  withMinLengthHint =
    if showMinLengthHint conf then Just $ minTransitionLength conf else Nothing
  }
  where
    (petri, cmd) = tries 1000 conf seed

tries :: Int -> Config -> Int -> (Net Place Transition, GraphvizCommand)
tries n conf seed = eval out
  where
    eval f = evalRand f $ mkStdGen seed
    out = do
      xs <- forM [1 .. n] $ const $ try conf
      let (l, pn) = maximumBy (comparing fst) $ concat xs
      if l >= minTransitionLength conf
        then (pn,) <$> oneOf (drawCommands conf)
        else out

try :: MonadRandom m => Config -> m [(Int, Net Place Transition)]
try conf = do
  let ps = [Place 1 .. Place (numPlaces conf)]
      ts = [Transition 1 .. Transition (numTransitions conf)]
  n <- Modelling.PetriNet.Reach.Roll.net
      ps
      ts
      (Modelling.PetriNet.Reach.Deadlock.capacity conf)
  return $ do
    let (no,yeah) = span (null . snd)
          $ take (maxTransitionLength conf + 1)
          $ zip [0 :: Int ..]
          $ deadlocks n
    guard $ not $ null yeah
    return (length no, n)

expl :: Net Int Int
expl =
  Net {
  places = S.fromList [1, 2, 3, 4, 5],
  transitions = S.fromList [1, 2, 3, 4, 5],
  connections = [
      ([1], 1, [1, 2, 3]),
      ([2], 2, [3, 4]),
      ([3], 3, [4, 5]),
      ([4], 4, [5, 1]),
      ([5], 5, [1, 2]),
      ([1, 2, 3, 4, 5], 7, [])
      ],
    Modelling.PetriNet.Reach.Type.capacity = Unbounded,
    start = State $ M.fromList [(1, 1), (2, 0), (3, 0), (4, 0), (5, 0)]
  }
