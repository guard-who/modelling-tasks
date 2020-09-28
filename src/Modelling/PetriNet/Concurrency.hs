{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Concurrency (
  findConcurrency, findConcurrencyTask, pickConcurrency, pickConcurrencyTask,
  ) where

import Modelling.PetriNet.Alloy          (petriNetFindConcur,petriNetPickConcur)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.Parser         (
  parseConcurrency, parsePetriLike,
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
findConcurrency indInst config@FindConcurrencyConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetFindConcur config)
  getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)

findConcurrencyTask :: String
findConcurrencyTask =
  "Which pair of transitions are in concurrency under the initial marking?"
  
pickConcurrency
  :: Int
  -> PickConcurrencyConfig
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent Object))]
pickConcurrency indInst config@PickConcurrencyConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetPickConcur config)
  conc <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicTask)
  return [conc,net]

pickConcurrencyTask :: String
pickConcurrencyTask =
   "Which of the following Petrinets does not have a concurrency?"

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe (Concurrent Object))
getNet f t inst gc = do
  pl <- except $ parsePetriLike f t inst
  dia <- drawNet pl gc
  if f == "defaultFlow" && t == "defaultTokens"
    then return (dia, Nothing)
    else do
    conc <- except $ parseConcurrency inst
    return (dia, Just conc)
