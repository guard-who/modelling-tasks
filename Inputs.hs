{-# LANGUAGE NamedFieldPuns #-}

module Inputs where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand )
--import Diagrams.Backend.SVG              (B)
--import Diagrams.Prelude                  (Diagram)
import FalsePetri
import Language.Alloy.Call               (AlloyInstance,getInstances)
import PetriDiagram                      (renderNet)
import PetriParser                       (runIParser,runAParser)
import PetriTex                          (runTex)
import Types

------------(finished ,finished,[(done,done)]) -- List needs to be done
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
        runTex petri 1
        let f = renderFalse tknChange flwChange petri
        list <- getInstances (Just 4) f
        wrongList 1 (graphLayout inp) list
  else
    print $ c
    
-- falseList :: Int -> GraphvizCommand -> [AlloyInstance] -> Either String [(Diagram B,Change)]
-- falseList _ _ []     = Right []
-- falseList i gc (inst:rs) = do
  -- fParsed <- runAParser inst
  -- case fParsed of
    -- (Left ferror,Left cError) -> return $ ferror ++ cError
    -- (Left ferror, _)          -> return ferror
    -- (_,Left cError)           -> return cError
    -- (Right fNet,Right change) -> do
      -- let net = return $ renderNet ("wrong"++show i) fNet gc
      -- return $ (net,change) : falseList (i+1) gc rs
      
wrongList :: Int -> GraphvizCommand -> [AlloyInstance] -> IO()
wrongList _ _ []         = print "finished"
wrongList i gc (inst:rs) = do
  fParsed <- runAParser inst
  case fParsed of
    (Left ferror,Left cError) -> print $ ferror ++ cError
    (Left ferror, _)          -> print ferror
    (_,Left cError)           -> print cError
    (Right fNet,Right change) -> do
      renderNet ("wrong"++show i) fNet gc
      print ("wrong"++show i++" : "++ show change)
      wrongList (i+1) gc rs
    
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
 | maxFlowOverall > constA                 = Just "maxFlowOverall not in bounds"
 | otherwise   = Nothing 
  where constA = 2 * places * transitions * maxFlowPerEdge
  
  