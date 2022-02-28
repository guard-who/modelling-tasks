{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Modelling.Auxiliary.Common       (oneOf)
import Modelling.Auxiliary.Output (
  LangM,
  OutputMonad (assertion, code, image, indent, paragraph, text),
  english,
  german,
  translate,
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Roll    (net)
import Modelling.PetriNet.Reach.Step    (executes, levels)
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
  mapState,
  mark,
  )

import Control.Monad                    (forM, forM_)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Random             (mkStdGen)
import Control.Monad.Trans.Random       (evalRand)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (minimumBy)
import Data.List.Extra                  (nubSort)
import Data.Ord                         (comparing)
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

reachTask
  :: (MonadIO m, OutputMonad m, Ord s, Ord t, Show s, Show t)
  => FilePath
  -> ReachInstance s t
  -> LangM m
reachTask path inst = do
  let n = petriNet inst
  (g, withoutPlaceNames) <- if showGoalNet inst
    then (,True) . Left
         <$> drawToFile True path (drawUsing inst) 0 (n { start = goal inst })
    else return (Right $ show $ goal inst, False)
  img <- drawToFile withoutPlaceNames path (drawUsing inst) (-1) n
  reportReachFor
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (withMinLengthHint inst)
    (Just g)

reportReachFor
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe (Either FilePath String)
  -> LangM m
reportReachFor img noLonger lengthHint minLengthHint mgoal = do
  paragraph $ text "Gesucht ist für das Petrinetz"
  image img
  paragraph $ case mgoal of
    Nothing -> paragraph $ text $ unlines [
      "eine Transitionsfolge,",
      "die zu einer Markierung ohne Nachfolger (Deadlock) führt."
      ]
    Just g -> do
      text "eine Transitionsfolge, durch die die folgende Markierung erreicht wird:"
      either image text g
  paragraph $ case noLonger of
    Nothing ->
      text "Geben Sie Ihre Lösung als (beliebig kurze oder lange) Auflistung der folgenden Art an:"
    Just maxL ->
      text $ concat [
        "Geben Sie Ihre Lösung als maximal ", show maxL,
        "-elementige Auflistung der folgenden Art an:"]
  code $ show $ map ShowTransition [Transition 1, Transition 2, Transition 3]
  paragraph $ text $ concat [
    "Wobei diese Angabe bedeuten soll, dass nach dem Schalten von ",
    show (ShowTransition $ Transition 1), ", danach ", show (ShowTransition $ Transition 2),
    ", und schließlich ", show (ShowTransition $ Transition 3),
    " (in genau dieser Reihenfolge), die gesuchte Markierung erreicht wird."
    ]
  (`mapM_` lengthHint) $ \len -> paragraph $ text
    [i|Hinweis: Es gibt eine Lösung mit nicht mehr als #{len} Transitionen.|]
  (`mapM_` minLengthHint) $ \len -> paragraph $ text
    [i|Hinweis: Es gibt keine Lösung mit weniger als #{len} Transitionen.|]

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
  mapM_ assertTransition . nubSort
  where
    assertTransition t = assertion (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a valid transition of the given Petri net?"
      german $ t' ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
    isValidTransition =  (`elem` transitions n)

reachEvaluation :: (MonadIO m, OutputMonad m, Show s, Show t, Ord s, Ord t)
  => FilePath
  -> ReachInstance s t
  -> [t]
  -> LangM m
reachEvaluation path inst ts = do
  isNoLonger (noLongerThan inst) ts
  paragraph $ text "Startmarkierung"
  indent $ text $ show (start n)
  out <- executes path False (drawUsing inst) n ts
  assertion (out == goal inst) $ text "Zielmarkierung erreicht?"
  where
    n = petriNet inst

isNoLonger :: OutputMonad m => Maybe Int -> [a] -> LangM m
isNoLonger mmaxL ts =
  forM_ mmaxL $ \maxL ->
    assertion (length ts <= maxL) $
      text $ unwords ["Nicht mehr als", show maxL, "Transitionen?"]

data ReachInstance s t = ReachInstance {
  drawUsing         :: GraphvizCommand,
  goal              :: State s,
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
  rejectLongerThan    = Nothing,
  showLengthHint      = True,
  showMinLengthHint   = True,
  showTargetNet       = True
  }

generateReach :: ReachConfig -> Int -> ReachInstance Place Transition
generateReach conf seed =
  let ps = [Place 1 .. Place (numPlaces conf)]
      tries = forM [1 :: Int .. 1000] $ const $ do
        n <- Modelling.PetriNet.Reach.Roll.net
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
    noLongerThan      = rejectLongerThan conf,
    petriNet          = petri,
    showGoalNet       = showTargetNet conf,
    withLengthHint    =
      if showLengthHint conf then Just $ maxTransitionLength conf else Nothing,
    withMinLengthHint =
      if showMinLengthHint conf then Just $ minTransitionLength conf else Nothing
    }
  where
    ts = [Transition 1 .. Transition (numTransitions conf)]
    eval f = evalRand f $ mkStdGen seed
