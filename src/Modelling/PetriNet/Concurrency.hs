{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig, checkPickConcurrencyConfig,
  findConcurrency, findConcurrencyTask,
  findConcurrencyTaskInstance,
  pickConcurrency, pickConcurrencyTask,
  pickConcurrencyTaskInstance,
  ) where

import Modelling.PetriNet.Alloy (
  getAlloyInstances, petriNetFindConcur, petriNetPickConcur,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Parser         (
  parseConcurrency,
  )
import Modelling.PetriNet.Types (
  BasicConfig(..), Concurrent, FindConcurrencyConfig(..),
  PickConcurrencyConfig(..),
  )

import Control.Monad.Trans.Except       (ExceptT, except)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig (..), defaultCallAlloyConfig,
  )
import Control.Monad (unless)

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
findConcurrencyTaskInstance = getNet "flow" "tokens"

pickConcurrencyTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent String))]
pickConcurrencyTaskInstance inst gc = do
  conc <- getNet "flow" "tokens" inst gc
  net <- getNet "defaultFlow" "defaultTokens" inst gc
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

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe (Concurrent String))
getNet = getNetWith parseConcurrency
