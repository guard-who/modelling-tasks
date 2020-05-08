{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.MatchToMath where

import Modelling.PetriNet.Alloy          (petriNetRnd, renderFalse)
import Modelling.PetriNet.Diagram        (drawNet,drawNets,prepNet)
import Modelling.PetriNet.LaTeX          (uebung,createPetriTex)
import Modelling.PetriNet.Parser         (convertPetri,runAParser)
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call               (AlloyInstance,getInstances)
import Text.LaTeX                        (LaTeX)

mainInput :: IO ()
mainInput = do
  (pls,trns,tknChange,flwChange) <- userInput
  let inp = defaultPetriConfig{places = pls, transitions = trns, tokenChangeOverall = tknChange
                         , flowChangeOverall = flwChange}
  let c = checkConfig inp
  if isNothing c
  then do
    _ <- matchToMath True inp
    print "finished"
  else
    print c
    
-- selectTask :: IO (Int)
-- selectTask = do
  -- putStr ("which of the following tasks would you like to generate? \n"
        -- ++"- 1: Mathematical to Petri (and vice versa) \n"
        -- ++"- 2: Conflicts in PetriNets"
        -- )
  -- task <- getLine
  -- return $ task :: Int

--True Task1 <-> False Task1a
matchToMath :: Bool -> PetriConfig -> IO (Diagram B, LaTeX, Either [(Diagram B, Change)] [(LaTeX, Change)])
matchToMath switch inp = do
  list <- getInstances (Just 1) (petriNetRnd inp)
  let out = convertPetri "tokens" (head list)
  case out of
    Left merror -> error merror
    Right petri -> do
      rightNet <- drawNet (prepNet petri) (graphLayout inp)
      let tex
           | switch    = uebung petri 1
           | otherwise = uebung petri 2
      let f = renderFalse petri inp
      fList <- getInstances (Just 3) f
      let (fNets,changes) = falseList fList []
      if switch then do
        fDia <- drawNets fNets (graphLayout inp)
        return (rightNet, tex, Left $ zip fDia changes)
      else do
        let fTex = map createPetriTex fNets
        return (rightNet, tex, Right $ zip fTex changes)

falseList :: [AlloyInstance] -> [Petri] -> ([Petri],[Change])
falseList [] _       = ([],[])
falseList (inst:rs) usedP = do
  let fParsed = runAParser inst
  case fParsed of
    (Left ferror,Left cError) -> error $ ferror ++ cError
    (Left ferror, _)          -> error ferror
    (_,Left cError)           -> error cError
    (Right fNet,Right change) -> do
      let rest@(rf,rc) = falseList rs (fNet:usedP)
      if fNet `elem` usedP 
      then rest
      else (fNet:rf,change:rc)
    
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
  
  
checkConfig :: PetriConfig -> Maybe String
checkConfig PetriConfig{places,transitions,atLeastActive,minTokensOverall,maxTokensOverall,maxTokensPerPlace
                 , minFlowOverall,maxFlowOverall,maxFlowPerEdge
                 , tokenChangeOverall, flowChangeOverall
                 , maxFlowChangePerEdge, maxTokenChangePerPlace}
 | places <= 0         = Just "There must at least be 1 Place"
 | places > 9          = Just "Places are to be picked in a range of 1 to 9"
 | transitions <= 0    = Just "There must at least be 1 Transition"
 | transitions > 9     = Just "Transitions are to be picked in a range of 1 to 9"
 | atLeastActive < 0    = Just "Least Activee Transitions must be at least 0"
 | minTokensOverall < 0  = Just "Tokens Overall must be at least 0"
 | maxTokensPerPlace < 0 = Just "Tokens per Place must be at least 0"
 | maxFlowPerEdge <= 0 = Just "Max Flow per Edge must be at least 1"
 | minFlowOverall < 0  = Just "Overall Flow must be at least 0"
 | tokenChangeOverall < 0     = Just "tokenChange can't be negative"
 | flowChangeOverall  < 0     = Just "flowChange can't be negative"
 | maxTokenChangePerPlace < 0 = Just "maxTokenChangePerPlace can't be negative"
 | maxFlowChangePerEdge < 0   = Just "maxFlowChangePerEdge can't be negative"
 | atLeastActive > transitions              = Just ("Least Activee Transitions must be lower than "
                                                ++"Transitions")
 | maxTokensOverall > places*maxTokensPerPlace = Just "choose a lower Max Token Overall"
 | maxTokensOverall < minTokensOverall         = Just "Min and Max Tokens Overall aren't fitting"
 | maxTokensPerPlace > maxTokensOverall        = Just ("Tokens Per Place must be lower than Max Tokens "
                                                ++ "Overall")
 | maxFlowOverall < minFlowOverall         = Just "Min and Max FLow Overall aren't fitting"
 | maxFlowOverall < maxFlowPerEdge         = Just "Flow Per Edge must be lower than Max Flow Overall"
 | maxFlowOverall > constA                 = Just "maxFlowOverall not in bounds"
 | maxTokenChangePerPlace > tokenChangeOverall = Just "maxTokenChangePerPlace must be at maximum Overall"
 | maxTokenChangePerPlace > maxTokensPerPlace    = Just "maxTokenChangePerPlace can't be higher than the maxTokensPerPlace"
 | maxFlowChangePerEdge > flowChangeOverall    = Just "maxFlowChangePerEdge must be lower than Overall"
 | maxFlowChangePerEdge > maxFlowPerEdge       = Just "maxFlowChangePerEdge can't be higher than the maxFlowPerEdge"
 | tokenChangeOverall > maxTokensOverall - minTokensOverall = Just "Stay within the Range of Tokens with the Change Overall"
 | flowChangeOverall > maxFlowOverall - minFlowOverall = Just "Stay within the Range of Flow with the Change Overall"
 | maxTokenChangePerPlace * places < tokenChangeOverall = Just "You can't have more Tokenchanges Overall than maxChange at all given Places together"
 | 2 * places * transitions * maxFlowChangePerEdge < flowChangeOverall = Just "You can't have more FlowCHange Overall than maxChange at all given Edges together"
 | otherwise   = Nothing 
  where constA = 2 * places * transitions * maxFlowPerEdge
  
  
