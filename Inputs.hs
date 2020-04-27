{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import PetriParser
import PetriDiagram        (renderNet)
import Types

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
      Left merror -> print merror
      Right petri -> renderNet petri (graphLayout inp)
  else 
    print "invalid Input"
    
checkInput :: Input -> Bool
checkInput Input{places,transitions,atLeastActiv,minTknsOverall,maxTknsOverall,maxTknsPerPlace,
                 minFlowOverall,maxFlowOverall,maxFlowPerEdge} = 
  places > 0 && transitions > 0 && atLeastActiv >= 0 && minTknsOverall >= 0 && maxTknsPerPlace >= 0 &&
  maxFlowPerEdge > 0 && minFlowOverall >= 0 &&
  atLeastActiv <= transitions &&
  maxTknsOverall <= places*maxTknsPerPlace &&
  maxTknsOverall >= minTknsOverall &&
  maxTknsPerPlace <= maxTknsOverall &&
  maxFlowOverall >= minFlowOverall &&
  maxFlowOverall >= maxFlowPerEdge 
  
  