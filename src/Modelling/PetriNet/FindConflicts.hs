module Modelling.PetriNet.FindConflicts where

import Modelling.PetriNet.Alloy          (petriNetConfl)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser         (convertPetri, parseConflict)
import Modelling.PetriNet.Types

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call               (getInstances)
import Text.LaTeX                        (LaTeX)

findConflicts :: PetriBasicConfig -> IO(Diagram B,LaTeX,Conflict)
findConflicts config = do
  list <- getInstances (Just 1) (petriNetConfl config)
  case convertPetri "flow" "tokens" (head list) of
    Left merror -> error merror
    Right petri -> do
      dia <- drawNet petri (graphLayout config)
      let tex = uebung petri 2 True
      case parseConflict (head list) of
        Left perror -> error perror
        Right confl -> return (dia, tex, confl)