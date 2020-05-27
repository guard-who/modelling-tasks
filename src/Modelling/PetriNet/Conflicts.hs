{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts 
  (findConflicts,pickConflicts,checkFindConfig,checkPickConfig) where

import Modelling.PetriNet.Alloy          (petriNetFindConfl,petriNetPickConfl)
import Modelling.PetriNet.BasicNetFunctions 
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (convertPetri, parseConflict)
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
          
checkFindConfig :: FindConflictConfig -> Maybe String
checkFindConfig f@FindConflictConfig{basicTask = BasicConfig{atLeastActive},changeTask}
 | atLeastActive < 1
  = Just "The parameter 'atLeastActive' must be at least 2 to create a conflict." 
 | otherwise = 
  checkBCConfig (basicTask(f :: FindConflictConfig)) changeTask
  
checkPickConfig :: PickConflictConfig -> Maybe String
checkPickConfig p@PickConflictConfig{basicTask = BasicConfig{atLeastActive},changeTask}
 | atLeastActive < 1
  = Just "The parameter 'atLeastActive' must be at least 2 to create a conflict." 
 | otherwise = 
  checkBCConfig (basicTask(p :: PickConflictConfig)) changeTask