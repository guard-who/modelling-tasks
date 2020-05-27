{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.BasicNetFunctions where

import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Parser         (convertPetri)
import Modelling.PetriNet.Types

import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Data.Maybe                        (isJust)
import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call               (AlloyInstance)

getDia :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B)
getDia st nd inst gc =
  case convertPetri st nd inst of
    Left merror -> error merror
    Right petri -> drawNet petri gc

checkBasicConfig :: BasicConfig -> Maybe String
checkBasicConfig BasicConfig{places,transitions,atLeastActive
                   , minTokensOverall,maxTokensOverall,maxTokensPerPlace
                   , minFlowOverall,maxFlowOverall,maxFlowPerEdge}
 | places <= 0
  = Just "The number of places must be positive."
 | places > 9
  = Just "Cannot deal with more than 9 places."
 | transitions <= 0
  = Just "The number of transitions must be positive."
 | transitions > 9
  = Just "Cannot deal with more than 9 transitions."
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
  
checkCConfig :: BasicConfig -> ChangeConfig -> Maybe String
checkCConfig basic@BasicConfig{atLeastActive} change
 | atLeastActive < 1
  = Just "The parameter 'atLeastActive' must be at least 2 to create the task." 
 | otherwise = do
  let c = checkBasicConfig basic
  if isJust c then c
  else checkChangeConfig basic change