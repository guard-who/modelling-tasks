{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts (findConflicts,pickConflicts) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (convertPetri, parseConflict)
import Modelling.PetriNet.Types          (Petri(..),Conflict,FindConflictConfig(..),PickConflictConfig(..),BasicConfig(..))

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)

--until i find a good working solution for LaTeX.hs taking Petri in function "uebung"
placeHoldPetri :: Petri
placeHoldPetri = Petri{initialMarking =[],trans=[]}

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
getNet st nd inst gc =
  case convertPetri st nd inst of
    Left merror -> error merror
    Right petri -> do
      dia <- drawNet petri gc
      if st == "defaultFlow" && nd == "defaultTokens" 
      then return (dia,Nothing)
      else
        case parseConflict inst of
          Left perror -> error perror
          Right confl -> return (dia, Just confl)