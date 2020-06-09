{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts 
  (findConflicts,pickConflicts) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (parseConflict,prepNodes)
import Modelling.PetriNet.Types          
  (placeHoldPetri,Conflict,FindConflictConfig(..),PickConflictConfig(..),BasicConfig(..))

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)


findConflicts :: FindConflictConfig -> IO(LaTeX,[(Diagram B, Maybe Conflict)])
findConflicts config@FindConflictConfig{basicTask} = do
  list <- getInstances (Just 1) (petriNetFindConfl config)
  confl <- getNet "flow" "tokens" (head list) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 2 True
  return (tex, [confl])
  
pickConflicts :: PickConflictConfig -> IO(LaTeX,[(Diagram B, Maybe Conflict)])
pickConflicts config@PickConflictConfig{basicTask} = do
  list <- getInstances (Just 1) (petriNetPickConfl config)
  confl <- getNet "flow" "tokens" (head list) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 2 False
  net <- getNet "defaultFlow" "defaultTokens" (head list) (graphLayout basicTask)
  return (tex, [confl,net])
        
getNet :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B, Maybe Conflict)
getNet f t inst gc =
  case prepNodes t inst of
    Left nerror -> error nerror
    Right nodes -> do
      dia <- drawNet f nodes inst gc
      if f == "defaultFlow" && t == "defaultTokens" 
      then return (dia,Nothing)
      else
        case parseConflict nodes inst of
          Left perror -> error perror
          Right confl -> return (dia, Just confl)
          
