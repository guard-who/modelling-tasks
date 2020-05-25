{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Concurrency (findConcurrency,pickConcurrency) where

import Modelling.PetriNet.Alloy          (petriNetFindConcur,petriNetPickConcur)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (convertPetri, parseConcurrency)
import Modelling.PetriNet.Types          (Petri(..),Concurrent,FindConcurrencyConfig(..),PickConcurrencyConfig(..),BasicConfig(..))

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)

placeHoldPetri :: Petri
placeHoldPetri = Petri{initialMarking =[],trans=[]}

findConcurrency :: FindConcurrencyConfig -> IO(LaTeX,[(Diagram B, Maybe Concurrent)])
findConcurrency config@FindConcurrencyConfig{basicTask} = do
  list <- getInstances (Just 1) (petriNetFindConcur config)
  confl <- getNet "flow" "tokens" (head list) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 3 True
  return (tex, [confl])
  
pickConcurrency :: PickConcurrencyConfig -> IO(LaTeX,[(Diagram B, Maybe Concurrent)])
pickConcurrency config@PickConcurrencyConfig{basicTask} = do
  list <- getInstances (Just 1) (petriNetPickConcur config)
  conc <- getNet "flow" "tokens" (head list) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 3 False
  net <- getNet "defaultFlow" "defaultTokens" (head list) (graphLayout basicTask)
  return (tex, [conc,net])

getNet :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B, Maybe Concurrent)
getNet st nd inst gc =
  case convertPetri st nd inst of
    Left merror -> error merror
    Right petri -> do
      dia <- drawNet petri gc
      if st == "defaultFlow" && nd == "defaultTokens" 
      then return (dia,Nothing)
      else
        case parseConcurrency inst of
          Left perror -> error perror
          Right conc -> return (dia, Just conc)