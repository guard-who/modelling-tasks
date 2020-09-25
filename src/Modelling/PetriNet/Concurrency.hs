{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Concurrency 
  (findConcurrency,pickConcurrency) where

import Modelling.PetriNet.Alloy          (petriNetFindConcur,petriNetPickConcur)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX          (uebung)
import Modelling.PetriNet.Parser         (
  parseConcurrency, parsePetriLike, prepNodes,
  )
import Modelling.PetriNet.Types          
  (placeHoldPetri,Concurrent,FindConcurrencyConfig(..),PickConcurrencyConfig(..),BasicConfig(..))

import Control.Monad.Trans.Except       (except, runExceptT)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)


findConcurrency :: Int -> FindConcurrencyConfig -> IO(LaTeX,[(Diagram B, Maybe Concurrent)])
findConcurrency indInst config@FindConcurrencyConfig{basicTask} = do
  list <- getInstances (Just (toInteger (indInst+1))) (petriNetFindConcur config)
  conc <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 3 True
  return (tex, [conc])
  
pickConcurrency :: Int -> PickConcurrencyConfig -> IO(LaTeX,[(Diagram B, Maybe Concurrent)])
pickConcurrency indInst config@PickConcurrencyConfig{basicTask} = do
  list <- getInstances (Just (toInteger (indInst+1))) (petriNetPickConcur config)
  conc <- getNet "flow" "tokens" (list !! indInst) (graphLayout basicTask)
  let tex = uebung placeHoldPetri 3 False
  net <- getNet "defaultFlow" "defaultTokens" (list !! indInst) (graphLayout basicTask)
  return (tex, [conc,net])

getNet :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B, Maybe Concurrent)
getNet f t inst gc =
  case prepNodes t inst of
    Left nerror -> error nerror
    Right nodes -> do
      edia <- runExceptT $ do
        pl <- except $ parsePetriLike f t inst
        drawNet pl gc
      dia  <- either error return edia
      if f == "defaultFlow" && t == "defaultTokens" 
      then return (dia,Nothing)
      else
        case parseConcurrency nodes inst of
          Left perror -> error perror
          Right conc -> return (dia, Just conc)