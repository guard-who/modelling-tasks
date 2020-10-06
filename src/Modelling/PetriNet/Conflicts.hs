{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts (
  findConflicts, findConflictsTask, pickConflicts, pickConflictsTask
  ) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.Parser        (
  parseConflict, parsePetriLike,
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), Conflict, FindConflictConfig(..), PickConflictConfig(..),
  )

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call              (AlloyInstance, Object, getInstances)

findConflicts
  :: Int
  -> FindConflictConfig
  -> ExceptT String IO (Diagram B, Maybe (Conflict Object))
findConflicts indInst config@FindConflictConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetFindConfl config)
  getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)

findConflictsTask :: [Char]
findConflictsTask =
  "Which of the following Petrinets doesn't have a conflict?"

pickConflicts
  :: Int
  -> PickConflictConfig
  -> ExceptT String IO [(Diagram B, Maybe (Conflict Object))]
pickConflicts indInst config@PickConflictConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetPickConfl config)
  confl <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicTask)
  return [confl,net]

pickConflictsTask :: [Char]
pickConflictsTask =
  "Which pair of transitions are in conflict under the initial marking?"

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe (Conflict Object))
getNet f t inst gc = do
  pl <- except $ parsePetriLike f t inst
  dia <- drawNet pl gc
  if f == "defaultFlow" && t == "defaultTokens"
    then return (dia, Nothing)
    else do
    conc <- except $ parseConflict inst
    return (dia, Just conc)
