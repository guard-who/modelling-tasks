{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand )
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import FalsePetri
import Language.Alloy.Call               (AlloyInstance,getInstances)
import PetriAlloy                        (petriNetRnd)
import PetriDiagram                      (drawNet,prepNet)
import PetriParser                       (convertPetri,runAParser)
import PetriTex                          (uebung)
import Text.LaTeX                        (LaTeX)
import Types

mainInput :: IO()
mainInput = do
  (pls,trns,tknChange,flwChange) <- userInput
  let inp = defaultInput{places = pls, transitions = trns, tokenChangeOverall = tknChange
                         , flowChangeOverall = flwChange}
  let c = checkInput inp
  if isNothing c
  then do
    _ <- mainTask1 inp
    print "finished"
  else
    print c

mainTask1 :: Input -> IO (Diagram B, LaTeX, [(Diagram B, Change)])
mainTask1 inp = do
  list <- getInstances (Just 1) (petriNetRnd inp)
  let out = convertPetri "tokens" (head list)
  case out of
    Left merror -> error merror
    Right petri -> do
      rightNet <- drawNet (prepNet petri) (graphLayout inp)
      let tex = uebung petri 1
      let f = renderFalse petri inp
      fList <- getInstances (Just 3) f
      fNets <- falseList (graphLayout inp) fList []
      return (rightNet, tex, fNets)

    
falseList :: GraphvizCommand -> [AlloyInstance] -> [Petri] -> IO [(Diagram B,Change)]
falseList _ [] _       = return []
falseList gc (inst:rs) usedP = do
  let fParsed = runAParser inst
  case fParsed of
    (Left ferror,Left cError) -> error $ ferror ++ cError
    (Left ferror, _)          -> error ferror
    (_,Left cError)           -> error cError
    (Right fNet,Right change) -> do
      rest <- falseList gc rs (fNet:usedP)
      if elem fNet usedP 
      then return $ rest
      else do
        net <- drawNet (prepNet fNet) gc
        return $ (net,change) : rest
    
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
checkInput Input{places,transitions,atLeastActiv,minTknsOverall,maxTknsOverall,maxTknsPerPlace
                 , minFlowOverall,maxFlowOverall,maxFlowPerEdge
                 , tokenChangeOverall, flowChangeOverall
                 , maxFlowChangePerEdge, maxTokenChangePerPlace}
 | places <= 0         = Just "There must at least be 1 Place"
 | places > 9          = Just "Places are to be picked in a range of 1 to 9"
 | transitions <= 0    = Just "There must at least be 1 Transition"
 | transitions > 9     = Just "Transitions are to be picked in a range of 1 to 9"
 | atLeastActiv < 0    = Just "Least Active Transitions must be at least 0"
 | minTknsOverall < 0  = Just "Tokens Overall must be at least 0"
 | maxTknsPerPlace < 0 = Just "Tokens per Place must be at least 0"
 | maxFlowPerEdge <= 0 = Just "Max Flow per Edge must be at least 1"
 | minFlowOverall < 0  = Just "Overall Flow must be at least 0"
 | tokenChangeOverall < 0     = Just "tokenChange can't be negative"
 | flowChangeOverall  < 0     = Just "flowChange can't be negative"
 | maxTokenChangePerPlace < 0 = Just "maxTokenChangePerPlace can't be negative"
 | maxFlowChangePerEdge < 0   = Just "maxFlowChangePerEdge can't be negative"
 | atLeastActiv > transitions              = Just ("Least Active Transitions must be lower than "
                                                ++"Transitions")
 | maxTknsOverall > places*maxTknsPerPlace = Just "choose a lower Max Token Overall"
 | maxTknsOverall < minTknsOverall         = Just "Min and Max Tokens Overall aren't fitting"
 | maxTknsPerPlace > maxTknsOverall        = Just ("Tokens Per Place must be lower than Max Tokens " 
                                                ++ "Overall")
 | maxFlowOverall < minFlowOverall         = Just "Min and Max FLow Overall aren't fitting"
 | maxFlowOverall < maxFlowPerEdge         = Just "Flow Per Edge must be lower than Max Flow Overall"
 | maxFlowOverall > constA                 = Just "maxFlowOverall not in bounds"
 | maxTokenChangePerPlace > tokenChangeOverall = Just "maxTokenChangePerPlace must be lower than Overall"
 | maxTokenChangePerPlace > maxTknsPerPlace    = Just "maxTokenChangePerPlace can't be higher than the maxTokensPerPlace"
 | maxFlowChangePerEdge > flowChangeOverall    = Just "maxFlowChangePerEdge must be lower than Overall"
 | maxFlowChangePerEdge > maxFlowPerEdge       = Just "maxFlowChangePerEdge can't be higher than the maxFlowPerEdge"
 | otherwise   = Nothing 
  where constA = 2 * places * transitions * maxFlowPerEdge
  
  