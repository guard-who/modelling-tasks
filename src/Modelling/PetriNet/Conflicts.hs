{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts (
  checkFindConflictConfig, checkPickConflictConfig
  ) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.BasicNetFunctions (
  checkBasicConfig, checkCConfig, checkChangeConfig,
  )
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.Parser        (
  parseConflict, parsePetriLike, simpleRenameWith
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), Conflict, FindConflictConfig(..), PickConflictConfig(..),
  traversePetriLike,
  )

import Control.Monad                    (unless)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call (
  AlloyInstance, getInstances,
  )

findConflicts
  :: Int
  -> FindConflictConfig
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflicts indInst config@FindConflictConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetFindConfl config)
  unless (length list > indInst) $ except $ Left "instance not available"
  getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)

findConflictsTask :: [Char]
findConflictsTask =
  "Which of the following Petrinets doesn't have a conflict?"

pickConflicts
  :: Int
  -> PickConflictConfig
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflicts indInst config@PickConflictConfig{basicTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetPickConfl config)
  when (null list) $ except $ Left "no instance available"
  confl <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicTask)
  return [confl,net]

pickConflictsTask :: [Char]
pickConflictsTask =
  "Which pair of transitions are in conflict under the initial marking?"

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig
  }
  | Just x <- checkCConfig basicConfig
  = Just x
  | Just x <- checkBasicConfig basicConfig
  = Just x
  | Just x <- checkChangeConfig basicConfig changeConfig
  = Just x
  | otherwise
  = Nothing

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig
  }
  | Just x <- checkBasicConfig basicConfig
  = Just x
  | Just x <- checkChangeConfig basicConfig changeConfig
  = Just x
  | otherwise
  = Nothing

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe Conflict)
getNet f t inst gc = do
  pl <- except $ parsePetriLike f t inst
  let rename = simpleRenameWith pl
  pl' <- except $ traversePetriLike rename pl
  dia <- drawNet id pl' gc
  if f == "defaultFlow" && t == "defaultTokens"
    then return (dia, Nothing)
    else do
    conc <- except $ parseConflict inst
    rconc <- except $ traverse rename conc
    return (dia, Just rconc)
