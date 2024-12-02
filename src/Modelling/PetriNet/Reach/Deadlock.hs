{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Deadlock.hs
-}
module Modelling.PetriNet.Reach.Deadlock where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common       (oneOf)
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Reach   (
  assertReachPoints,
  isNoLonger,
  reportReachFor,
  transitionsValid,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (deadlocks, deadlocks', executes, successors)
import Modelling.PetriNet.Reach.Type (
  Capacity (Unbounded),
  Net (..),
  Place (..),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  State (State),
  Transition (..),
  TransitionsList (TransitionsList),
  bimapNet,
  example,
  )

import Control.Applicative              (Alternative)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  Rated,
  english,
  german,
  translate,
  yesNo,
  )
import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Data.Bifunctor                   (Bifunctor (second))
import Data.Either.Combinators          (whenRight)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (guard, replicateM)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random             (MonadRandom, evalRand, mkStdGen)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (maximumBy)
import Data.Maybe                       (fromMaybe)
import Data.Ord                         (comparing)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

verifyDeadlock
  :: (OutputCapable m, Show a, Show t, Ord t, Ord a)
  => DeadlockInstance a t
  -> LangM m
verifyDeadlock = validate Default . petriNet

deadlockTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputCapable m,
    Show s,
    Show t
    )
  => FilePath
  -> DeadlockInstance s t
  -> LangM m
deadlockTask path inst = do
  lift (drawToFile True path (drawUsing inst) 0 (petriNet inst))
  $>>= \img -> reportReachFor
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (withMinLengthHint inst)
    Nothing

deadlockInitial :: DeadlockInstance s Transition -> TransitionsList
deadlockInitial = TransitionsList . reverse . S.toList . transitions . petriNet

deadlockSyntax
  :: OutputCapable m
  => DeadlockInstance Place Transition
  -> [Transition]
  -> LangM m
deadlockSyntax = transitionsValid . petriNet

deadlockEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> DeadlockInstance Place Transition
  -> [Transition]
  -> Rated m
deadlockEvaluation path deadlock ts =
  isNoLonger (noLongerThan deadlockInstance) ts
  $>> executes path (drawUsing deadlockInstance) n (map ShowTransition ts)
  $>>= \eitherOutcome ->
    whenRight eitherOutcome (\outcome ->
      yesNo (null $ successors n outcome)
      $ translate $ do
          english "Reached marking has no successors?"
          german "Zielmarkierung hat keine Nachfolger?"
      )
  $>> assertReachPoints
    aSolution
    (const $ null . successors n)
    minLength
    deadlockInstance
    ts
    eitherOutcome
  where
    deadlockInstance = toShowDeadlockInstance deadlock
    n = petriNet deadlockInstance
    aSolution
      | showSolution deadlockInstance
      = Just $ show $ TransitionsList $ deadlockSolution deadlock
      | otherwise
      = Nothing

deadlockSolution :: Ord s => DeadlockInstance s t -> [t]
deadlockSolution = reverse . snd . head . concat . deadlocks' . petriNet

data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showSolution      :: Bool,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Maybe Int
  } deriving (Generic, Read, Show, Typeable)

bimapDeadlockInstance
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> DeadlockInstance s t
  -> DeadlockInstance a b
bimapDeadlockInstance f g DeadlockInstance {..} = DeadlockInstance {
    drawUsing         = drawUsing,
    minLength         = minLength,
    noLongerThan      = noLongerThan,
    petriNet          = bimapNet f g petriNet,
    showSolution      = showSolution,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint
    }

toShowDeadlockInstance
  :: DeadlockInstance Place Transition
  -> DeadlockInstance ShowPlace ShowTransition
toShowDeadlockInstance = bimapDeadlockInstance ShowPlace ShowTransition

data DeadlockConfig = DeadlockConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  postconditionsRange :: (Int, Maybe Int),
  preconditionsRange  :: (Int, Maybe Int),
  printSolution       :: Bool,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool
  }
  deriving (Generic, Read, Show, Typeable)

defaultDeadlockConfig :: DeadlockConfig
defaultDeadlockConfig =
  DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Deadlock.capacity = Unbounded,
  drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
  maxTransitionLength = 10,
  minTransitionLength = 8,
  postconditionsRange = (0, Nothing),
  preconditionsRange  = (0, Nothing),
  printSolution       = False,
  rejectLongerThan    = Nothing,
  showLengthHint      = True,
  showMinLengthHint   = True
  }

defaultDeadlockInstance :: DeadlockInstance Place Transition
defaultDeadlockInstance = DeadlockInstance {
  drawUsing         = Circo,
  minLength         = 6,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showSolution      = False,
  withLengthHint    = Just 9,
  withMinLengthHint = Just 6
  }

generateDeadlock :: DeadlockConfig -> Int -> DeadlockInstance Place Transition
generateDeadlock conf@DeadlockConfig {..} seed = DeadlockInstance {
  drawUsing         = cmd,
  minLength         = minTransitionLength,
  noLongerThan      = rejectLongerThan,
  petriNet          = petri,
  showSolution      = printSolution,
  withLengthHint    =
    if showLengthHint then Just maxTransitionLength else Nothing,
  withMinLengthHint =
    if showMinLengthHint then Just minTransitionLength else Nothing
  }
  where
    (petri, cmd) = tries 1000 conf seed

tries :: Int -> DeadlockConfig -> Int -> (Net Place Transition, GraphvizCommand)
tries n conf seed = eval out
  where
    eval f = evalRand f $ mkStdGen seed
    out = do
      xs <- replicateM n $ try conf
      let (l, pn) = maximumBy (comparing fst) $ concat xs
      if l >= minTransitionLength conf
        then (pn,) <$> oneOf (drawCommands conf)
        else out

try :: MonadRandom m => DeadlockConfig -> m [(Int, Net Place Transition)]
try conf = do
  let ps = [Place 1 .. Place (numPlaces conf)]
      ts = [Transition 1 .. Transition (numTransitions conf)]
  n <- netLimits vLow vHigh nLow nHigh
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
  where
    fixMaximum = second (min (numPlaces conf) . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum $ preconditionsRange conf
    (nLow, nHigh) = fixMaximum $ postconditionsRange conf

exampleInstance :: Net Int Int
exampleInstance =
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
