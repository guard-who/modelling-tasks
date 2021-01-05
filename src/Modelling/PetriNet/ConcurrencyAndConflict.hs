{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  checkFindConcurrencyConfig, checkFindConflictConfig,
  checkPickConcurrencyConfig, checkPickConflictConfig,
  findConcurrency, findConcurrencyTask,
  findConflicts, findConflictsTask,
  findTaskInstance,
  pickConcurrency, pickConcurrencyTask,
  pickConflicts, pickConflictsTask,
  pickTaskInstance,
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
  AlloyInstance, CallAlloyConfig (..), Object, defaultCallAlloyConfig,
  )

findConcurrencyTask :: String
findConcurrencyTask =
  "Which pair of transitions are in concurrency under the initial marking?"

findConflictsTask :: String
findConflictsTask =
  "Which of the following Petrinets doesn't have a conflict?"

pickConcurrencyTask :: String
pickConcurrencyTask =
   "Which of the following Petri nets does not have a concurrency?"

pickConflictsTask :: String
pickConflictsTask =
  "Which pair of transitions are in conflict under the initial marking?"

findConcurrency
  :: Int
  -> FindConcurrencyConfig
  -> ExceptT String IO (Diagram B, Maybe (Concurrent String))
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  (\c -> graphLayout $ basicConfig (c :: FindConcurrencyConfig))

findConflicts
  :: Int
  -> FindConflictConfig
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflicts = taskInstance
  findTaskInstance
  petriNetFindConfl
  parseConflict
  (\c -> graphLayout $ basicConfig (c :: FindConflictConfig))

pickConcurrency
  :: Int
  -> PickConcurrencyConfig
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent String))]
pickConcurrency  = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  (\c -> graphLayout $ basicConfig (c :: PickConcurrencyConfig))

pickConflicts
  :: Int
  -> PickConflictConfig
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflicts = taskInstance
  pickTaskInstance
  petriNetPickConfl
  parseConflict
  (\c -> graphLayout $ basicConfig (c :: PickConflictConfig))

taskInstance
  :: (f -> AlloyInstance -> GraphvizCommand -> ExceptT String IO a)
  -> (config -> String)
  -> f
  -> (config -> GraphvizCommand)
  -> Int
  -> config
  -> ExceptT String IO a
taskInstance taskF alloyF parseF layoutF indInst config = do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (alloyF config)
  unless (length list > indInst) $ except $ Left "instance not available"
  taskF parseF (list !! indInst) (layoutF config)

findTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe (t String))
findTaskInstance parseF = getNetWith parseF "flow" "tokens"

pickTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe (t String))]
pickTaskInstance parseF inst gc = do
  confl <- getNetWith parseF "flow" "tokens" inst gc
  net   <- getNetWith parseF "defaultFlow" "defaultTokens" inst gc
  return [confl,net]

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
