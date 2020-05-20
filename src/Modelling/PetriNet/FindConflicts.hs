module Modelling.PetriNet.FindConflicts (findConflicts) where

import Modelling.PetriNet.Alloy          (petriNetConfl)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (convertPetri, parseConflict)
import Modelling.PetriNet.Types          (Petri(..),Conflict,BasicConfig(..))

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Language.Alloy.Call               (getInstances,AlloyInstance)
import Text.LaTeX                        (LaTeX)

--until i find a good working solution for LaTeX.hs taking Petri in function "uebung"
placeHoldPetri :: Petri
placeHoldPetri = Petri{initialMarking =[],trans=[]}

findConflicts :: Bool -> BasicConfig -> IO(LaTeX,[(Diagram B, Maybe Conflict)])
findConflicts sw config = do
  list <- getInstances (Just 1) (petriNetConfl config)
  confl <- getNet "flow" "tokens" (head list) (graphLayout config)
  let tex = uebung placeHoldPetri 2 sw
  if sw 
  then return (tex, [confl])
  else do
    net <- getNet "defaultFlow" "defaultTokens" (head list) (graphLayout config)
    return (tex, [confl,net])
        
getNet :: String -> String -> AlloyInstance -> GraphvizCommand -> IO (Diagram B, Maybe Conflict)
getNet st nd inst gc = do
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