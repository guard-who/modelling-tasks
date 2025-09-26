{-# LANGUAGE QuasiQuotes #-}
-- | Common validation logic for ActivityDiagram Petri-based tasks
module Modelling.ActivityDiagram.Auxiliary.PetriValidation (
  validatePetriConfig,
  validateBasePetriConfig
) where

import qualified Modelling.ActivityDiagram.Config as Config (
  AdConfig (activityFinalNodes, flowFinalNodes, actionLimits, forkJoinPairs, cycles),
  )

import Control.Applicative (Alternative ((<|>)))
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate (iii)

-- | Base validation logic common to multiple Petri-based configurations
validateBasePetriConfig
  :: Config.AdConfig
  -> (Int, Maybe Int)  -- countOfPetriNodesBounds
  -> Maybe Integer  -- maxInstances
  -> Maybe Bool  -- presenceOfSinkTransitionsForFinals
  -> Maybe String
validateBasePetriConfig adConfig countOfPetriNodesBounds maxInstances presenceOfSinkTransitionsForFinals
  | Config.activityFinalNodes adConfig > 1
  = Just "There is at most one 'activityFinalNode' allowed."
  | Config.activityFinalNodes adConfig >= 1 && Config.flowFinalNodes adConfig >= 1
  = Just "There is no 'flowFinalNode' allowed if there is an 'activityFinalNode'."
  | fst countOfPetriNodesBounds < 0
  = Just "'countOfPetriNodesBounds' must not contain negative values"
  | Just high <- snd countOfPetriNodesBounds, fst countOfPetriNodesBounds > high
  = Just "the second value of 'countOfPetriNodesBounds' must not be smaller than its first value"
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | Just False <- presenceOfSinkTransitionsForFinals,
    fst (Config.actionLimits adConfig) + Config.forkJoinPairs adConfig < 1
    = Just "The option 'presenceOfSinkTransitionsForFinals = Just False' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing

-- | Common validation logic for configurations that share Petri-related parameters
validatePetriConfig
  :: Config.AdConfig
  -> (Int, Maybe Int)  -- countOfPetriNodesBounds
  -> Maybe Integer  -- maxInstances
  -> [GraphvizCommand]  -- petriLayout
  -> Maybe Bool  -- auxiliaryPetriNodeAbsent
  -> Maybe Bool  -- presenceOfSinkTransitionsForFinals
  -> Maybe Bool  -- withActivityFinalInForkBlocks
  -> Maybe String
validatePetriConfig
  adConfig
  countOfPetriNodesBounds
  maxInstances
  petriLayout
  auxiliaryPetriNodeAbsent
  presenceOfSinkTransitionsForFinals
  withActivityFinalInForkBlocks =
  validateBasePetriConfig adConfig countOfPetriNodesBounds maxInstances presenceOfSinkTransitionsForFinals
  <|> validatePetriConfigSpecific
  where
    validatePetriConfigSpecific
      | auxiliaryPetriNodeAbsent == Just True && Config.cycles adConfig > 0
      = Just [iii|
        Setting the parameter 'auxiliaryPetriNodeAbsent' to True
        prohibits having more than 0 cycles
        |]
      | withActivityFinalInForkBlocks == Just False && Config.activityFinalNodes adConfig > 1
        = Just "Setting the parameter 'withActivityFinalInForkBlocks' to False prohibits having more than 1 'activityFinalNodes'"
      | withActivityFinalInForkBlocks == Just True && Config.activityFinalNodes adConfig == 0
        = Just "Setting the parameter 'withActivityFinalInForkBlocks' to True implies that there are 'activityFinalNodes'"
      | null petriLayout
        = Just "The parameter 'petriLayout' can not be the empty list"
      | any (`notElem` [Dot, Neato, TwoPi, Circo, Fdp]) petriLayout
        = Just "The parameter 'petriLayout' can only contain the options Dot, Neato, TwoPi, Circo and Fdp"
      | otherwise
        = Nothing
