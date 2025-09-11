{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Reach.hs
-}
module Modelling.PetriNet.Reach.Reach where

import qualified Control.Monad.Trans              as Monad (lift)
import qualified Data.Set                         as S (toList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Data.Data                        (Data)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile, isPetriDrawable)
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
import Control.Monad                    (forM, when)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (findM, maybeM, whenJust)
import Control.OutputCapable.Blocks (
  ArticleToUse (IndefiniteArticle),
  GenericOutputCapable (assertion, code, image, indent, paragraph, text),
  LangM,
  MinimumThreshold (MinimumThreshold),
  OutputCapable,
  Rated,
  english,
  german,
  printSolutionAndAssertMinimum,
  translate,
  yesNo,
  )
import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Control.Monad.Random             (mkStdGen)
import Control.Monad.Trans.Random       (evalRandT)
import Data.Bifunctor                   (Bifunctor (second))
import Data.Either.Combinators          (whenRight)
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

verifyReach :: (Ord a, Ord t, OutputCapable m, Show a, Show t)
  => ReachInstance a t
  -> LangM m
verifyReach inst = do
  let n = petriNet (netGoal inst)
  validate Default n
  validate Default $ n { start = goal (netGoal inst) }
  pure ()

reachTask
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
  -> ReachInstance s t
  -> LangM m
reachTask path inst = do
  if showGoalNet inst
    then (,True) . Left
    <$> lift (drawToFile True path (drawUsing (netGoal inst)) (n { start = goal (netGoal inst) }))
    else pure (Right $ show $ goal (netGoal inst), False)
  $>>= \(g, withoutPlaceNames) ->
    lift (drawToFile withoutPlaceNames path (drawUsing (netGoal inst)) n)
  $>>= \img -> reportReachFor
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (withMinLengthHint inst)
    (Just g)
  where
    n = petriNet (netGoal inst)

reportReachFor
  :: OutputCapable m
  => FilePath
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe (Either FilePath String)
  -> LangM m
reportReachFor img noLonger lengthHint minLengthHint maybeGoal = do
  paragraph $ translate $ do
    english "For the Petri net"
    german "Gesucht ist für das Petrinetz"
  image img
  paragraph $ case maybeGoal of
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
        show maxL," steps:"]
      german $ concat [
        "Geben Sie Ihre Lösung als maximal ", show maxL,
        "-schrittige Auflistung der folgenden Art an:"]
  let (t1, t2, t3) = (Transition 1, Transition 2, Transition 3)
      showT = show . ShowTransition
      (st1, st2, st3) = (showT t1, showT t2, showT t3)
  code $ show $ TransitionsList [t1, t2, t3]
  paragraph $ translate $ do
    english $ concat [
      "Where giving these three steps means that after firing ",
      st1, ", then ", st2, ", and finally ", st3,
      " (in exactly this order), the sought marking is reached."
      ]
    german $ concat [
      "Wobei die Angabe dieser drei Schritte bedeuten soll, dass nach dem Schalten von ",
      st1, ", danach ", st2, ", und schließlich ", st3,
      " (in genau dieser Reihenfolge), die gesuchte Markierung erreicht wird."
      ]
  whenJust lengthHint $ \count -> when (noLonger /= Just count) $ paragraph $ translate $ do
    english [i|Hint: There is a solution with not more than #{count} steps.|]
    german [i|Hinweis: Es gibt eine Lösung mit nicht mehr als #{count} Schritten.|]
  whenJust minLengthHint $ \count -> paragraph $ translate $ do
    english [i|Hint: There is no solution with less than #{count} steps.|]
    german [i|Hinweis: Es gibt keine Lösung mit weniger als #{count} Schritten.|]
  hoveringInformation
  pure ()

reachInitial :: ReachInstance s Transition -> TransitionsList
reachInitial = TransitionsList . reverse . S.toList . transitions . petriNet . netGoal

reachSyntax
  :: OutputCapable m
  => ReachInstance s Transition
  -> [Transition]
  -> LangM m
reachSyntax inst ts =
  do transitionsValid (petriNet (netGoal inst)) ts
     isNoLonger (noLongerThan inst) ts
     pure ()

transitionsValid :: OutputCapable m => Net s Transition -> [Transition] -> LangM m
transitionsValid n =
  traverse_ assertTransition . nubSort
  where
    assertTransition t = assertion (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a transition of the given Petri net?"
      german $ t' ++ " ist eine Transition des gegebenen Petrinetzes?"
    isValidTransition =  (`elem` transitions n)

reachEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> ReachInstance Place Transition
  -> [Transition]
  -> Rated m
reachEvaluation path reach ts =
  do paragraph $ translate $ do
       english "Start marking:"
       german "Startmarkierung:"
     indent $ text $ show (start n)
     pure ()
  $>> executes path (drawUsing (netGoal reachInstance)) n (map ShowTransition ts)
  $>>= \eitherOutcome -> whenRight eitherOutcome (\outcome ->
    yesNo (outcome == goal (netGoal reachInstance)) $ translate $ do
      english "Reached target marking?"
      german "Zielmarkierung erreicht?"
    )
  $>> assertReachPoints
    aSolution
    ((==) . goal . netGoal)
    minLength
    reachInstance
    ts
    eitherOutcome
  where
    reachInstance = toShowReachInstance reach
    n = petriNet (netGoal reachInstance)
    aSolution
      | showSolution reach = Just $ show $ TransitionsList $ reachSolution reach
      | otherwise = Nothing

netGoalSolution :: Ord s => NetGoal s t -> [t]
netGoalSolution netGoal = reverse $ snd $ head $ concatMap
  (filter $ (== goal netGoal) . fst)
  $ levels' $ petriNet netGoal

reachSolution :: Ord s => ReachInstance s t -> [t]
reachSolution inst = netGoalSolution (netGoal inst)

assertReachPoints
  :: OutputCapable m
  => Maybe String
  -> (i -> a -> Bool)
  -> (i -> Int)
  -> i
  -> [b]
  -> Either Int a
  -> Rated m
assertReachPoints aCorrectSolution p size inst ts eitherOutcome = do
  let points = either
        partly
        (\x -> if p inst x then 1 else partly $ length ts)
        eitherOutcome
  printSolutionAndAssertMinimum
    (MinimumThreshold $ 1 % 3)
    IndefiniteArticle
    aCorrectSolution
    points
  where
    partly x = partiallyCorrect x $ size inst
    partiallyCorrect x y = min 0.6 $
      if y == 0
      then 0
      else toInteger x % toInteger y

isNoLonger :: OutputCapable m => Maybe Int -> [a] -> LangM m
isNoLonger maybeMaxLength ts =
  whenJust maybeMaxLength $ \maxLength ->
    assertion (length ts <= maxLength) $ translate $ do
      english $ unwords [
        "At most",
        show maxLength,
        "steps provided?"
        ]
      german $ unwords [
        "Höchstens",
        show maxLength,
        "Schritte angegeben?"
        ]

data ReachInstance s t = ReachInstance {
  netGoal           :: NetGoal s t,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  showGoalNet       :: Bool,
  showSolution      :: Bool,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Maybe Int
  } deriving (Generic, Read, Show, Typeable, Data)

data NetGoal s t = NetGoal {
  drawUsing         :: GraphvizCommand,
  petriNet          :: Net s t,
  goal              :: State s
  } deriving (Generic, Read, Show, Typeable, Data)

bimapReachInstance
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> ReachInstance s t
  -> ReachInstance a b
bimapReachInstance f g ReachInstance {..} = ReachInstance {
    netGoal           = bimapNetGoal f g netGoal,
    minLength         = minLength,
    noLongerThan      = noLongerThan,
    showGoalNet       = showGoalNet,
    showSolution      = showSolution,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint
    }

bimapNetGoal
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> NetGoal s t
  -> NetGoal a b
bimapNetGoal f g NetGoal {..} = NetGoal {
    drawUsing = drawUsing,
    goal      = mapState f goal,
    petriNet  = bimapNet f g petriNet
    }

toShowReachInstance
  :: ReachInstance Place Transition
  -> ReachInstance ShowPlace ShowTransition
toShowReachInstance = bimapReachInstance ShowPlace ShowTransition

toShowNetGoal
  :: NetGoal Place Transition
  -> NetGoal ShowPlace ShowTransition
toShowNetGoal = bimapNetGoal ShowPlace ShowTransition

data ReachConfig = ReachConfig {
  netGoalConfig       :: NetGoalConfig,
  printSolution       :: Bool,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool,
  showTargetNet       :: Bool
  }
  deriving (Generic, Read, Show, Typeable)

data NetGoalConfig = NetGoalConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  postconditionsRange :: (Int, Maybe Int),
  preconditionsRange  :: (Int, Maybe Int)
  }
  deriving (Generic, Read, Show, Typeable)

defaultReachConfig :: ReachConfig
defaultReachConfig = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces           = 4,
    numTransitions      = 4,
    Modelling.PetriNet.Reach.Reach.capacity = Unbounded,
    drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
    maxTransitionLength = 8,
    minTransitionLength = 6,
    postconditionsRange = (0, Nothing),
    preconditionsRange  = (0, Nothing)
    },
  printSolution       = False,
  rejectLongerThan    = Nothing,
  showLengthHint      = True,
  showMinLengthHint   = True,
  showTargetNet       = True
  }

defaultReachInstance :: ReachInstance Place Transition
defaultReachInstance = ReachInstance {
  netGoal = NetGoal {
    drawUsing         = Circo,
    petriNet          = fst example,
    goal              = snd example
    },
  minLength         = 12,
  noLongerThan      = Nothing,
  showGoalNet       = True,
  showSolution      = False,
  withLengthHint    = Just 12,
  withMinLengthHint = Nothing
}

generateNetGoal
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => NetGoalConfig
  -> Int
  -> m (NetGoal Place Transition)
generateNetGoal NetGoalConfig {..} seed = do
  let ps = [Place 1 .. Place numPlaces]
      tries = forM [1 :: Int .. 1000] $ const $ do
        n <- netLimits vLow vHigh nLow nHigh
            ps
            ts
            capacity
        return $ do
          (l,zs) <-
            take (maxTransitionLength + 1) $ zip [0 :: Int ..] $ levels n
          z' <- zs
          let d = sum $ do
                p <- ps
                return $ abs (mark (start n) p - mark z' p)
          return ((negate l, d), (n, z'))
      out = do
        xs <- tries
        let ((l, _), pn) = minimumBy (comparing fst) $ concat xs
        if negate l >= minTransitionLength
          then do
            maybeM out (pure . (pn,))
            $ findM (Monad.lift . isPetriDrawable (fst pn)) drawCommands
          else out

  ((petri, state), cmd) <- eval out

  pure $ NetGoal {
    drawUsing   = cmd,
    goal        = state,
    petriNet    = petri
    }

  where
    fixMaximum = second (min numPlaces . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum preconditionsRange
    (nLow, nHigh) = fixMaximum postconditionsRange
    ts = [Transition 1 .. Transition numTransitions]
    eval f = evalRandT f $ mkStdGen seed

generateReach
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => ReachConfig
  -> Int
  -> m (ReachInstance Place Transition)
generateReach ReachConfig {..} seed = do
  netGoal <- generateNetGoal netGoalConfig seed
  pure $ ReachInstance {
    netGoal           = netGoal,
    minLength         = minTransitionLength netGoalConfig,
    noLongerThan      = rejectLongerThan,
    showGoalNet       = showTargetNet,
    showSolution      = printSolution,
    withLengthHint    =
      if showLengthHint then Just $ maxTransitionLength netGoalConfig else Nothing,
    withMinLengthHint =
      if showMinLengthHint then Just $ minTransitionLength netGoalConfig else Nothing
    }
