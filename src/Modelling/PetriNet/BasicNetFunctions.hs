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
 | tokenChangeOverall > maxTokensOverall - minTokensOverall
  = Just "With 'tokenChangeOverall', stay within the range of tokens overall."
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
 | flowChangeOverall > maxFlowOverall - minFlowOverall
  = Just "With 'flowChangeOverall', stay within the range of flow overall."
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

prohibitHideTransitionNames :: BasicConfig -> Maybe String
prohibitHideTransitionNames bc
  | hideTransitionNames bc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing

checkConfigForPick :: Bool -> BasicConfig -> ChangeConfig -> Maybe String
checkConfigForPick useDifferent basic change
  = checkBasicConfig basic
  <|> checkChangeConfig basic change
  <|> checkGraphLayouts useDifferent basic

checkGraphLayouts :: Bool -> BasicConfig -> Maybe String
checkGraphLayouts useDifferent bc
  | useDifferent && length (graphLayout bc) <= 2
  = Just "useDifferentGraphLayouts may only be set if graphLayout is set to at least two layouts"
  | otherwise
  = Nothing
