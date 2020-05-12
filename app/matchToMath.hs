module Main where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)

main :: IO ()
main = do
  (pls,trns,tknChange,flwChange) <- userInput
  let inp = defaultPetriTask1Config{basicTask1= defaultPetriBasicConfig{places = pls, transitions = trns}
                         , tokenChangeOverall = tknChange
                         , flowChangeOverall = flwChange}
  let ct = checkTask1Config inp
  let c  = checkBasicConfig (basicTask1 inp)
  if isNothing c
  then if isNothing ct 
       then do
         _ <- matchToMath True inp
         print "finished"
       else 
         print ct
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
