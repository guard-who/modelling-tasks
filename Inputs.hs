{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import FalsePetri
import PetriParser
import PetriDiagram        (renderNet)
import Types

userInput :: IO()
userInput = do
  putStr "Anzahl der Stellen: "
  pls <- getLine
  putStr "Anzahl der Transitionen: "
  trns <- getLine
  let inp = defaultInput{ places = read pls, transitions = read trns }
  let c = checkInput inp
  if c == Nothing
  then do  
    out <- runIParser inp
    case out of 
      Left merror -> print merror
      Right petri -> do
        renderNet "right" petri (graphLayout inp)
        let f = renderFalse petri
        fPetri <- runAParser f
        case fPetri of
          Left ferror -> print ferror
          Right fNet -> renderNet "wrong" fNet (graphLayout inp)
  else
    print $ c
  
checkInput :: Input -> Maybe String
checkInput Input{places,transitions,atLeastActiv,minTknsOverall,maxTknsOverall,maxTknsPerPlace,
                 minFlowOverall,maxFlowOverall,maxFlowPerEdge}
 | places <= 0         = Just "There must at least be 1 Place"
 | transitions <= 0    = Just "There must at least be 1 Transition"
 | atLeastActiv < 0    = Just "Least Active Transitions must be at least 0"
 | minTknsOverall < 0  = Just "Tokens Overall must be at least 0"
 | maxTknsPerPlace < 0 = Just "Tokens per Place must be at least 0"
 | maxFlowPerEdge <= 0 = Just "Max Flow per Edge must be at least 1"
 | minFlowOverall < 0  = Just "Overall Flow must be at least 0"
 | atLeastActiv > transitions              = Just ("Least Active Transitions must be lower than "
                                                ++"Transitions")
 | maxTknsOverall > places*maxTknsPerPlace = Just "choose a lower Max Token Overall"
 | maxTknsOverall < minTknsOverall         = Just "Min and Max Tokens Overall aren't fitting"
 | maxTknsPerPlace > maxTknsOverall        = Just ("Tokens Per Place must be lower than Max Tokens " 
                                                ++ "Overall")
 | maxFlowOverall < minFlowOverall         = Just "Min and Max FLow Overall aren't fitting"
 | maxFlowOverall < maxFlowPerEdge         = Just "Flow Per Edge must be lower than Max Flow Overall"
 | otherwise   = Nothing 
  
  