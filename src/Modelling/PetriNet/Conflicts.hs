{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts 
  (findConflicts,pickConflicts) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX          (uebung)
import Modelling.PetriNet.Parser         (parseConflict,prepNodes)
import Modelling.PetriNet.Types          
  (placeHoldPetri,Conflict,FindConflictConfig(..),PickConflictConfig(..),BasicConfig(..))

import Control.Monad.Trans.Except       (runExceptT)
import Diagrams.Backend.Rasterific       (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)


findConflicts :: Int -> FindConflictConfig -> IO(LaTeX,[(Diagram B, Maybe Conflict)])
findConflicts indInst config@FindConflictConfig{basicTask} = do
  list <- getInstances (Just (toInteger (indInst+1))) (petriNetFindConfl config)
  confl <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 2 True
  return (tex, [confl])
  
pickConflicts :: Int -> PickConflictConfig -> IO(LaTeX,[(Diagram B, Maybe Conflict)])
pickConflicts indInst config@PickConflictConfig{basicTask} = do
  list <- getInstances (Just (toInteger (indInst+1))) (petriNetPickConfl config)
  confl <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 2 False
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicTask)
  return (tex, [confl,net])
        
getNet :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B, Maybe Conflict)
getNet f t inst gc =
  case prepNodes t inst of
    Left nerror -> error nerror
    Right nodes -> do
      edia <- runExceptT $ drawNet f t inst gc
      dia  <- either error return edia
      if f == "defaultFlow" && t == "defaultTokens" 
      then return (dia,Nothing)
      else
        case parseConflict nodes inst of
          Left perror -> error perror
          Right confl -> return (dia, Just confl)
          
