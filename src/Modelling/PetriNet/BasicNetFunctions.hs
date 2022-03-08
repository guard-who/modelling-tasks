{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.BasicNetFunctions where

import Modelling.PetriNet.Types

import Control.Applicative              (Alternative ((<|>)))

checkBasicConfig :: BasicConfig -> Maybe String
checkBasicConfig BasicConfig{
  atLeastActive,
  graphLayout,
  maxFlowOverall,
  maxFlowPerEdge,
  maxTokensOverall,
  maxTokensPerPlace,
  minFlowOverall,
  minTokensOverall,
  places,
  transitions
  }
 | places <= 0
  = Just "The number of places must be positive."
 | places > 8
  = Just "Cannot deal with more than 8 places."
 | transitions <= 0
  = Just "The number of transitions must be positive."
 | transitions > 8
  = Just "Cannot deal with more than 8 transitions."
 | atLeastActive < 0
  = Just "The parameter 'atLeastActive' must be non-negative."
 | atLeastActive > transitions
  = Just "There cannot be more active transitions than there are transitions."
 | minTokensOverall < 0
  = Just "The parameter 'minTokensOverall' must be non-negative."
 | maxTokensOverall < minTokensOverall
  = Just "The parameter 'minTokensOverall' must not be larger than 'maxTokensOverall'."
 | maxTokensPerPlace < 0
  = Just "The parameter 'maxTokensPerPlace' must be non-negative."
 | maxTokensPerPlace > maxTokensOverall
  = Just "The parameter 'maxTokensPerPlace' must not be larger than 'maxTokensOverall'."
 | maxTokensOverall > places * maxTokensPerPlace
  = Just "The parameter 'maxTokensOverall' is set unreasonably high, given the per-place parameter."
 | minFlowOverall < 0
  = Just "The parameter 'minFlowOverall' must be non-negative."
 | maxFlowOverall < minFlowOverall
  = Just "The parameter 'minFlowOverall' must not be larger than 'maxFlowOverall'."
 | maxFlowPerEdge <= 0
  = Just "The parameter 'maxFlowPerEdge' must be positive."
 | maxFlowOverall < maxFlowPerEdge
  = Just "The parameter 'maxFlowPerEdge' must not be larger than 'maxFlowOverall'."
 | maxFlowOverall > 2 * places * transitions * maxFlowPerEdge
  = Just "The parameter 'maxFlowOverall' is set unreasonably high, given the other parameters."
 | transitions + places > 1 + minFlowOverall
  = Just "The number of transitions and places exceeds 'minFlowOverall' too much to create a connected net."
 | null graphLayout
 = Just "At least one graph layout needs to be provided."
 | otherwise
  = Nothing

checkChangeConfig :: BasicConfig -> ChangeConfig -> Maybe String
checkChangeConfig BasicConfig
                   {places,transitions
                   , minTokensOverall,maxTokensOverall,maxTokensPerPlace
                   , minFlowOverall,maxFlowOverall,maxFlowPerEdge}
                ChangeConfig
                   {tokenChangeOverall, flowChangeOverall
                   , maxFlowChangePerEdge, maxTokenChangePerPlace}

 | tokenChangeOverall < 0
  = Just "The parameter 'tokenChangeOverall' must be non-negative."
 | maxTokenChangePerPlace < 0
  = Just "The parameter 'maxTokenChangePerPlace' must be non-negative."
 | maxTokenChangePerPlace > tokenChangeOverall
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'tokenChangeOverall'."
 | maxTokenChangePerPlace > maxTokensPerPlace
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'maxTokensPerPlace'."
 | tokenChangeOverall > 2 * maxTokensOverall
  = Just "The parameter 'tokenChangeOverall' is set unreasonably high, given the maximal tokens overall."
 | maxTokenChangePerPlace * places < tokenChangeOverall
  = Just "The parameter 'tokenChangeOverall' is set unreasonably high, given the per-place parameter."
 | flowChangeOverall < 0
  = Just "The parameter 'flowChangeOverall' must be non-negative."
 | maxFlowChangePerEdge < 0
  = Just "The parameter 'maxFlowChangePerEdge' must be non-negative."
 | maxFlowChangePerEdge > flowChangeOverall
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'flowChangeOverall'."
 | maxFlowChangePerEdge > maxFlowPerEdge
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'maxFlowPerEdge'."
 | flowChangeOverall > 2 * maxFlowOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonable high, given the maximal flow overall."
 | 2 * places * transitions * maxFlowChangePerEdge < flowChangeOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonably high, given the other parameters."
 | otherwise
  = Nothing

checkCConfig :: BasicConfig  -> Maybe String
checkCConfig BasicConfig{atLeastActive}
 | atLeastActive < 2
  = Just "The parameter 'atLeastActive' must be at least 2 to create the task."
 | otherwise = Nothing

checkConfigForFind :: BasicConfig -> ChangeConfig -> Maybe String
checkConfigForFind basic change =
  checkCConfig basic
  <|> prohibitHideTransitionNames basic
  <|> checkBasicConfig basic
  <|> checkChangeConfig basic change

prohibitHidePlaceNames :: BasicConfig -> Maybe String
prohibitHidePlaceNames bc
  | hidePlaceNames bc
  = Just "Place names are required for this task type."
  | otherwise
  = Nothing

prohibitHideTransitionNames :: BasicConfig -> Maybe String
prohibitHideTransitionNames bc
  | hideTransitionNames bc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing

checkConfigForPick :: Bool -> Int -> BasicConfig -> ChangeConfig -> Maybe String
checkConfigForPick useDifferent wrongInstances basic change
  = checkBasicConfig basic
  <|> checkChangeConfig basic change
  <|> checkGraphLayouts useDifferent wrongInstances basic

checkGraphLayouts :: Bool -> Int -> BasicConfig -> Maybe String
checkGraphLayouts useDifferent wrongInstances bc
  | useDifferent && length (graphLayout bc) <= wrongInstances
  = Just "The parameter 'graphLayout' has to contain more entries than the number of 'wrongInstances' if 'useDifferentGraphLayouts' is set."
  | otherwise
  = Nothing

checkConflictConfig :: BasicConfig -> ConflictConfig -> Maybe String
checkConflictConfig bc ConflictConfig {
  addConflictCommonPreconditions,
  withConflictDistractors,
  conflictDistractorAddExtraPreconditions,
  conflictDistractorOnlyConflictLike,
  conflictDistractorOnlyConcurrentLike
  }
  | Just True <- withConflictDistractors
  , conflictDistractorOnlyConflictLike == conflictDistractorOnlyConcurrentLike
  = Just "Either 'conflictDistractorOnlyConflictLike' or 'conflictDistractorOnlyConcurrentLike' hast to be set!"
  | Just True <- withConflictDistractors
  , places bc < minPlaces
  = Just $ "Your current conflict configuration requires at least "
    ++ show minPlaces ++ " places."
  | Just True <- withConflictDistractors
  , transitions bc < minTransitions
  = Just $ "Your current conflict configuration requires at least "
    ++ show minTransitions ++ " transitions."
  | Just True <- withConflictDistractors
  = Nothing
  | Just {} <- conflictDistractorAddExtraPreconditions
  = Just "The parameter 'conflictDistractorAddExtraPreconditions' can only be set, if 'withConflictDistractors' is enforced."
  | conflictDistractorOnlyConflictLike
  = Just "The parameter 'conflictDistractorOnlyConflictLike' can only be set, if 'withConflictDistractors' is enforced."
  | conflictDistractorOnlyConcurrentLike
  = Just "The parameter 'conflictDistractorOnlyConcurrentLike' can only be set, if 'withConflictDistractors' is enforced."
  | otherwise
  = Nothing
  where
    minPlaces = (2 +) . sum $
      [1 |  Just True == addConflictCommonPreconditions]
      ++ [1 | Just True ==  withConflictDistractors]
      ++ [1
         | Just True == withConflictDistractors
         , Just True == conflictDistractorAddExtraPreconditions]
    minTransitions = 2 + sum
      [2 | Just True == withConflictDistractors]
