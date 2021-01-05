{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  checkFindConcurrencyConfig, checkFindConflictConfig,
  checkPickConcurrencyConfig, checkPickConflictConfig,
  findConcurrency, findConcurrencyTask,
  findConcurrencyTaskInstance,
  findConflicts, findConflictsTask,
  findConflictsTaskInstance,
  pickConcurrency, pickConcurrencyTask,
  pickConcurrencyTaskInstance,
  pickConflicts, pickConflictsTask,
  pickConflictsTaskInstance,
  ) where

import Modelling.PetriNet.Alloy (
  getAlloyInstances,
  petriNetFindConcur, petriNetFindConfl,
  petriNetPickConcur, petriNetPickConfl,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram       (getNetWith)
import Modelling.PetriNet.Parser        (
  parseConcurrency,
  parseConflict,
  )
import Modelling.PetriNet.Types         (
  BasicConfig (..),
  Concurrent, Conflict,
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PickConcurrencyConfig (..), PickConflictConfig (..),
  )

import Control.Monad                    (unless)
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig (..), defaultCallAlloyConfig,
  )
findConcurrency
  :: Int
  -> FindConcurrencyConfig
  -> ExceptT String IO (Diagram B, Maybe (Concurrent String))
findConcurrency indInst config@FindConcurrencyConfig{ basicConfig } = do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetFindConcur config)
  unless (length list > indInst) $ except $ Left "instance not available"
  findConcurrencyTaskInstance (list !! indInst) (graphLayout basicConfig)

findConcurrencyTask :: String
findConcurrencyTask =
  "Which pair of transitions are in concurrency under the initial marking?"

pickConcurrency
  :: Int
  -> PickConcurrencyConfig
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent String))]
pickConcurrency indInst config@PickConcurrencyConfig{ basicConfig } = do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetPickConcur config)
  unless (length list > indInst) $ except $ Left "instance not available"
  pickConcurrencyTaskInstance (list !! indInst) (graphLayout basicConfig)

pickConcurrencyTask :: String
pickConcurrencyTask =
   "Which of the following Petri nets does not have a concurrency?"

findConcurrencyTaskInstance :: AlloyInstance -> GraphvizCommand -> ExceptT String IO (Diagram B, Maybe (Concurrent String))
findConcurrencyTaskInstance = getNetWith parseConcurrency "flow" "tokens"

pickConcurrencyTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent String))]
pickConcurrencyTaskInstance inst gc = do
  conc <- getNetWith parseConcurrency "flow" "tokens" inst gc
  net <- getNetWith parseConcurrency "defaultFlow" "defaultTokens" inst gc
  return [conc,net]

checkFindConcurrencyConfig :: FindConcurrencyConfig -> Maybe String
checkFindConcurrencyConfig FindConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConcurrencyConfig :: PickConcurrencyConfig -> Maybe String
checkPickConcurrencyConfig PickConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig

findConflicts
  :: Int
  -> FindConflictConfig
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflicts indInst config@FindConflictConfig{basicConfig} = do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetFindConfl config)
  unless (length list > indInst) $ except $ Left "instance not available"
  findConflictsTaskInstance (list !! indInst) (graphLayout basicConfig)

findConflictsTask :: String
findConflictsTask =
  "Which of the following Petrinets doesn't have a conflict?"

pickConflicts
  :: Int
  -> PickConflictConfig
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflicts indInst  config@PickConflictConfig{basicConfig}= do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetPickConfl config)
  unless (length list > indInst) $ except $ Left "instance not available"
  pickConflictsTaskInstance (list !! indInst) (graphLayout basicConfig)

findConflictsTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflictsTaskInstance = getNet "flow" "tokens"

pickConflictsTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflictsTaskInstance inst gc = do
  confl <- getNet "flow" "tokens" inst gc
  net   <- getNet "defaultFlow" "defaultTokens" inst gc
  return [confl,net]

pickConflictsTask :: String
pickConflictsTask =
  "Which pair of transitions are in conflict under the initial marking?"

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe Conflict)
getNet = getNetWith parseConflict
