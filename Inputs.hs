{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import FalsePetri
import PetriParser
import PetriDiagram        (renderNet)
import Types

------------(finished ,finished,[(WIP,WIP)])
--main :: IO(Diagram B, LaTeX, [(Diagram B, Change)])

mainInput :: IO()
mainInput = do
  (pls,trns,tknChange,flwChange) <- userInput
  let inp = defaultInput{ places = pls, transitions = trns }
  let c = checkInput inp
  if c == Nothing
  then do  
    out <- runIParser inp
    case out of 
      Left merror -> print merror
      Right petri -> do
        renderNet "right" petri (graphLayout inp)
        let f = renderFalse tknChange flwChange petri
        fParsed <- runAParser f
        case fParsed of
          (Left ferror,Left cError) -> print $ ferror ++ cError
          (Left ferror, _)          -> print ferror
          (_,Left cError)           -> print cError
          (Right fNet,Right change) -> do
            renderNet "wrong" fNet (graphLayout inp)
            print change
  else
    print $ c
    
userInput :: IO (Int,Int,Int,Int)
userInput = do   
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  putStr "TokenChange: "
  tknCh <- getLine
  putStr "FlowChange: "
  flwCh <- getLine
  return (read pls, read trns, read tknCh, read flwCh)
  
  
checkInput :: Input -> Maybe String
checkInput Input{places,transitions,atLeastActiv,minTknsOverall,maxTknsOverall,maxTknsPerPlace,
                 minFlowOverall,maxFlowOverall,maxFlowPerEdge}
 | places <= 0         = Just "There must at least be 1 Place"
 | places > 9          = Just "Places are to be picked in a range of 1 to 9"
 | transitions <= 0    = Just "There must at least be 1 Transition"
 | transitions > 9     = Just "Transitions are to be picked in a range of 1 to 9"
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
  
  