{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Reach.hs
-}
module Modelling.PetriNet.Reach.Reach where

import qualified Data.Set                         as S (toList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common       (oneOf)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (executes, levels, levels')
import Modelling.PetriNet.Reach.Type (
  Capacity (Unbounded),
  Net (start, transitions),
  Place (..),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  State,
  Transition (..),
  TransitionsList (TransitionsList),
  bimapNet,
  example,
  mapState,
  mark,
  )

import Control.Applicative              (Alternative)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (forM, unless, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Extra              (whenJust)
import Control.Monad.Output (
  GenericOutputMonad (assertion, code, image, indent, paragraph, refuse, text),
  LangM,
  OutputMonad,
  Rated,
  english,
  german,
  translate,
  yesNo,
  )
import Control.Monad.Output.Generic (
  ($>>),
  ($>>=),
  )
import Control.Monad.Random             (mkStdGen)
import Control.Monad.Trans.Random       (evalRand)
import Data.Bifunctor                   (Bifunctor (second))
import Data.Either                      (isRight)
import Data.Foldable                    (traverse_)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (minimumBy)
import Data.List.Extra                  (nubSort)
import Data.Maybe                       (fromMaybe)
import Data.Ord                         (comparing)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

verifyReach :: (OutputMonad m, Show a, Show t, Ord t, Ord a)
  => ReachInstance a t
  -> LangM m
verifyReach inst = do
  let n = petriNet inst
  validate Default n
  validate Default $ n { start = goal inst }
  pure ()

reachTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputMonad m,
    Show s,
    Show t
    )
  => FilePath
  -> ReachInstance s t
  -> LangM m
reachTask path inst = do
  if showGoalNet inst
    then (,True) . Left
    <$> lift (drawToFile True path (drawUsing inst) 0 (n { start = goal inst }))
    else pure (Right $ show $ goal inst, False)
  $>>= \(g, withoutPlaceNames) ->
    lift (drawToFile withoutPlaceNames path (drawUsing inst) (-1) n)
  $>>= \img -> reportReachFor
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (withMinLengthHint inst)
    (Just g)
  where
    n = petriNet inst

reportReachFor
  :: OutputMonad m
  => FilePath
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe (Either FilePath String)
  -> LangM m
reportReachFor img noLonger lengthHint minLengthHint mgoal = do
  paragraph $ translate $ do
    english "For the Petri net"
    german "Gesucht ist für das Petrinetz"
  image img
  paragraph $ case mgoal of
    Nothing -> translate $ do
      english "a transition sequence is sought which leads to a marking without successors (i.e., to a deadlock)."
      german "eine Transitionsfolge, die zu einer Markierung ohne Nachfolger (also zu einem Deadlock) führt."
    Just g -> do
      translate $ do
        english "a transition sequence is sought which leads to the following marking:"
        german "eine Transitionsfolge, durch welche die folgende Markierung erreicht wird:"
      paragraph $ either image text g
      pure ()
  paragraph $ case noLonger of
    Nothing -> translate $ do
      english "State your answer as an (arbitrarily short or long) sequence of the following kind:"
      german "Geben Sie Ihre Lösung als (beliebig kurze oder lange) Auflistung der folgenden Art an:"
    Just maxL -> translate $ do
      english $ concat [
        "State your solution as a sequence of the following kind that does not exceed ",
        show maxL," elements:"]
      german $ concat [
        "Geben Sie Ihre Lösung als maximal ", show maxL,
        "-elementige Auflistung der folgenden Art an:"]
  let (t1, t2, t3) = (Transition 1, Transition 2, Transition 3)
      showT = show . ShowTransition
      (st1, st2, st3) = (showT t1, showT t2, showT t3)
  code $ show $ TransitionsList [t1, t2, t3]
  paragraph $ translate $ do
    english $ concat [
      "Where this statement means that after firing ",
      st1, ", then ", st2, ", and finally ", st3,
      " (in exactly this order), the sought marking is reached."
      ]
    german $ concat [
      "Wobei diese Angabe bedeuten soll, dass nach dem Schalten von ",
      st1, ", danach ", st2, ", und schließlich ", st3,
      " (in genau dieser Reihenfolge), die gesuchte Markierung erreicht wird."
      ]
  whenJust lengthHint $ \len -> paragraph $ translate $ do
    english [i|Hint: There is a solution with not more than #{len} transitions.|]
    german [i|Hinweis: Es gibt eine Lösung mit nicht mehr als #{len} Transitionen.|]
  whenJust minLengthHint $ \len -> paragraph $ translate $ do
    english [i|Hint: There is no solution with less than #{len} transitions.|]
    german [i|Hinweis: Es gibt keine Lösung mit weniger als #{len} Transitionen.|]
  hoveringInformation
  pure ()

reachInitial :: ReachInstance s Transition -> TransitionsList
reachInitial = TransitionsList . reverse . S.toList . transitions . petriNet

reachSyntax
  :: OutputMonad m
  => ReachInstance s Transition
  -> [Transition]
  -> LangM m
reachSyntax inst = transitionsValid (petriNet inst)

transitionsValid :: OutputMonad m => Net s Transition -> [Transition] -> LangM m
transitionsValid n =
  traverse_ assertTransition . nubSort
  where
    assertTransition t = assertion (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a valid transition of the given Petri net?"
      german $ t' ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
    isValidTransition =  (`elem` transitions n)

reachEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputMonad m,
    Show s,
    Show t
    )
  => FilePath
  -> ReachInstance s t
  -> [t]
  -> Rated m
reachEvaluation path inst ts =
  do isNoLonger (noLongerThan inst) ts
     paragraph $ translate $ do
       english "Start marking:"
       german "Startmarkierung:"
     indent $ text $ show (start n)
     pure ()
  $>> executes path (drawUsing inst) n ts
  $>>= \eout -> when (isRight eout) (
    yesNo (eout == Right (goal inst)) $ translate $ do
      english "Reached target marking?"
      german "Zielmarkierung erreicht?"
    )
  $>> assertReachPoints ((==) . goal) minLength inst ts eout
  where
    n = petriNet inst

reachSolution :: Ord s => ReachInstance s t -> [t]
reachSolution inst = reverse $ snd $ head $ concatMap
  (filter $ (== goal inst) . fst)
  $ levels' $ petriNet inst

assertReachPoints
  :: OutputMonad m
  => (i -> a -> Bool)
  -> (i -> Int)
  -> i
  -> [b]
  -> Either Int a
  -> Rated m
assertReachPoints p len inst ts eout = do
  let points = either
        partly
        (\x -> if p inst x then 1 else partly $ length ts)
        eout
  unless (points >= 1 % 3) $ refuse $ pure ()
  pure points
  where
    partly x = partiallyCorrect x $ len inst
    partiallyCorrect x y = min 0.6 $
      if y == 0
      then 0
      else toInteger x % toInteger y

isNoLonger :: OutputMonad m => Maybe Int -> [a] -> LangM m
isNoLonger maybeMaxLength ts =
  whenJust maybeMaxLength $ \maxLength ->
    assertion (length ts <= maxLength) $ translate $ do
      english $ unwords [
        "Not more than",
        show maxLength,
        "transitions provided?"
        ]
      german $ unwords [
        "Nicht mehr als",
        show maxLength,
        "Transitionen angegeben?"
        ]

data ReachInstance s t = ReachInstance {
  drawUsing         :: GraphvizCommand,
  goal              :: State s,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showGoalNet       :: Bool,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Maybe Int
  } deriving (Generic, Read, Show, Typeable)

bimapReachInstance
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> ReachInstance s t
  -> ReachInstance a b
bimapReachInstance f g x = ReachInstance {
    drawUsing         = drawUsing x,
    goal              = mapState f (goal x),
    minLength         = minLength x,
    noLongerThan      = noLongerThan x,
    petriNet          = bimapNet f g (petriNet x),
    showGoalNet       = showGoalNet x,
    withLengthHint    = withLengthHint x,
    withMinLengthHint = withMinLengthHint x
    }

toShowReachInstance
  :: ReachInstance Place Transition
  -> ReachInstance ShowPlace ShowTransition
toShowReachInstance = bimapReachInstance ShowPlace ShowTransition

data ReachConfig = ReachConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  postconditionsRange :: (Int, Maybe Int),
  preconditionsRange  :: (Int, Maybe Int),
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool,
  showTargetNet       :: Bool
  }
  deriving (Generic, Read, Show, Typeable)

defaultReachConfig :: ReachConfig
defaultReachConfig = ReachConfig {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Reach.capacity = Unbounded,
  drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
  maxTransitionLength = 8,
  minTransitionLength = 6,
  postconditionsRange = (0, Nothing),
  preconditionsRange  = (0, Nothing),
  rejectLongerThan    = Nothing,
  showLengthHint      = True,
  showMinLengthHint   = True,
  showTargetNet       = True
  }

defaultReachInstance :: ReachInstance Place Transition
defaultReachInstance = ReachInstance {
  drawUsing         = Circo,
  goal              = snd example,
  minLength         = 12,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showGoalNet       = True,
  withLengthHint    = Just 12,
  withMinLengthHint = Nothing
}

generateReach :: ReachConfig -> Int -> ReachInstance Place Transition
generateReach conf seed =
  let ps = [Place 1 .. Place (numPlaces conf)]
      tries = forM [1 :: Int .. 1000] $ const $ do
        n <- netLimits vLow vHigh nLow nHigh
            ps
            ts
            (Modelling.PetriNet.Reach.Reach.capacity conf)
        return $ do
          (l,zs) <-
            take (maxTransitionLength conf + 1) $ zip [0 :: Int ..] $ levels n
          z' <- zs
          let d = sum $ do
                p <- ps
                return $ abs (mark (start n) p - mark z' p)
          return ((negate l, d), (n, z'))
      out = do
        xs <- tries
        let ((l, _), pn) =  minimumBy (comparing fst) $ concat xs
        if negate l >= minTransitionLength conf
          then (pn,) <$> oneOf (drawCommands conf)
          else out
      ((petri, state), cmd) = eval out
  in ReachInstance {
    drawUsing         = cmd,
    goal              = state,
    minLength         = minTransitionLength conf,
    noLongerThan      = rejectLongerThan conf,
    petriNet          = petri,
    showGoalNet       = showTargetNet conf,
    withLengthHint    =
      if showLengthHint conf then Just $ maxTransitionLength conf else Nothing,
    withMinLengthHint =
      if showMinLengthHint conf then Just $ minTransitionLength conf else Nothing
    }
  where
    fixMaximum = second (min (numPlaces conf) . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum $ preconditionsRange conf
    (nLow, nHigh) = fixMaximum $ postconditionsRange conf
    ts = [Transition 1 .. Transition (numTransitions conf)]
    eval f = evalRand f $ mkStdGen seed
