{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.Find (
  FindInstance (..),
  checkCConfig,
  checkConfigForFind,
  findInitial,
  findTaskInstance,
  lToFind,
  toFindEvaluation,
  toFindSyntax,
  ) where

import qualified Data.Bimap                       as BM (lookup)

import Modelling.Auxiliary.Common       (Object)
import Modelling.Auxiliary.Output (
  addPretext,
  )
import Modelling.PetriNet.Diagram (
  getNet,
  )
import Modelling.PetriNet.Reach.Type (
  ShowTransition (ShowTransition),
  Transition (Transition),
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  DrawSettings (..),
  PetriLike,
  checkBasicConfig,
  checkChangeConfig,
  shuffleNames,
  transitionPairShow,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Lens                     (makeLensesFor)
import Control.Monad.Output (
  LangM',
  Language (English, German),
  OutputMonad (..),
  continueOrAbort,
  english,
  german,
  localise,
  translate,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  )
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)
import Data.Map                         (Map)
import Language.Alloy.Call (
  AlloyInstance,
  )
import GHC.Generics                     (Generic)

data FindInstance a = FindInstance {
  drawFindWith :: !DrawSettings,
  toFind :: !a,
  net :: !(PetriLike String),
  numberOfPlaces :: !Int,
  numberOfTransitions :: !Int,
  showSolution :: !Bool
  }
  deriving (Functor, Generic, Read, Show)

makeLensesFor [("toFind", "lToFind")] ''FindInstance

findInitial :: (Transition, Transition)
findInitial = (Transition 0, Transition 1)

toFindSyntax
  :: OutputMonad m
  => Bool
  -> Int
  -> (Transition, Transition)
  -> LangM' m ()
toFindSyntax withSol n (fi, si) = addPretext $ do
  assertTransition fi
  assertTransition si
  where
    assert = continueOrAbort withSol
    assertTransition t = assert (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a valid transition of the given Petri net?"
      german $ t' ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
    isValidTransition (Transition x) = x >= 1 && x <= n

findTaskInstance
  :: (RandomGen g, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> RandT g (ExceptT String IO) (PetriLike String, t String)
findTaskInstance f inst = do
  (pl, t) <- lift $ getNet f inst
  (pl', mapping) <- shuffleNames pl
  t'  <- lift $ (`BM.lookup` mapping) `mapM` t
  return (pl', t')

toFindEvaluation
  :: (Num a, OutputMonad m)
  => Map Language String
  -> Bool
  -> (Transition, Transition)
  -> (Transition, Transition)
  -> LangM' m (Maybe String, a)
toFindEvaluation what withSol (ft, st) (fi, si) = do
  let correct = ft == fi && st == si || ft == si && st == fi
      points = if correct then 1 else 0
      msolutionString =
        if withSol
        then Just $ show $ transitionPairShow (ft, st)
        else Nothing
  assert correct $ translate $ do
    english $ "Given transitions " ++ localise English what ++ "?"
    german $ "Die angegebenen Transitionen " ++ localise German what ++ "?"
  return (msolutionString, points)
  where
    assert = continueOrAbort withSol

checkCConfig :: BasicConfig -> Maybe String
checkCConfig BasicConfig { atLeastActive }
 | atLeastActive < 2
  = Just "The parameter 'atLeastActive' must be at least 2 to create the task."
 | otherwise = Nothing

checkConfigForFind :: BasicConfig -> ChangeConfig -> Maybe String
checkConfigForFind basic change =
  checkCConfig basic
  <|> prohibitHideTransitionNames basic
  <|> checkBasicConfig basic
  <|> checkChangeConfig basic change

prohibitHideTransitionNames :: BasicConfig -> Maybe String
prohibitHideTransitionNames bc
  | hideTransitionNames bc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing
