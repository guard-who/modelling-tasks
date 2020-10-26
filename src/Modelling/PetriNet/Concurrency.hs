{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig, checkPickConcurrencyConfig,
  findConcurrency, findConcurrencyTask, pickConcurrency, pickConcurrencyTask,
  ) where

import Modelling.PetriNet.Alloy          (petriNetFindConcur,petriNetPickConcur)
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Parser         (
  parseConcurrency, parseRenamedPetriLike,
  )
import Modelling.PetriNet.Types (
  BasicConfig(..), Concurrent, FindConcurrencyConfig(..),
  PickConcurrencyConfig(..),
  )

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (AlloyInstance, Object, getInstances)

findConcurrency
  :: Int
  -> FindConcurrencyConfig
  -> ExceptT String IO (Diagram B, Maybe (Concurrent Object))
findConcurrency indInst config@FindConcurrencyConfig{ basicConfig } = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetFindConcur config)
  getNet "flow" "tokens" (list !! indInst) (graphLayout basicConfig)

findConcurrencyTask :: String
findConcurrencyTask =
  "Which pair of transitions are in concurrency under the initial marking?"
  
pickConcurrency
  :: Int
  -> PickConcurrencyConfig
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent Object))]
pickConcurrency indInst config@PickConcurrencyConfig{ basicConfig } = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetPickConcur config)
  conc <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicConfig)
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicConfig)
  return [conc,net]

pickConcurrencyTask :: String
pickConcurrencyTask =
   "Which of the following Petri nets does not have a concurrency?"

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
  -> ExceptT String IO (Diagram B, Maybe (Concurrent Object))
getNet f t inst gc = do
  pl <- except $ parseRenamedPetriLike f t inst
  dia <- drawNet id pl gc
  if f == "defaultFlow" && t == "defaultTokens"
    then return (dia, Nothing)
    else do
    conc <- except $ parseConcurrency inst
    return (dia, Just conc)
