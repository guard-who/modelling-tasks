{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi, Neato))
import Text.Read
import PetriParser
import PetriDiagram        (renderNet)
import AuxFunctions
import Types
import PetriAlloy
import Language.Alloy.Call

userInput :: IO()
userInput = do
  putStr "Anzahl der Stellen: "
  pls <- getLine
  putStr "Anzahl der Transitionen: "
  trns <- getLine
  let inp = defaultInput{ places = read pls :: Int , transitions = read trns :: Int}
  if checkInput inp 
  then do
    out <- runPParser inp
    case out of 
      Left error -> print error
      Right petri -> renderNet petri (graphLayout inp)
  else 
    print "invalid Input"
    
checkInput :: Input -> Bool
checkInput Input{places,transitions,atLeastActv,minTknsOv,maxTknsPPs,maxFlowPEdge} = 
  places > 0 && transitions > 0 && minTknsOv >= 0 && maxFlowPEdge > 0 && atLeastActv >= 0 &&
  minTknsOv <= places*maxTknsPPs &&
  maxTknsPPs <= minTknsOv &&
  atLeastActv <= transitions
  