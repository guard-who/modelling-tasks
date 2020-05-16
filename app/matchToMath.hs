module Main (main) where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (pls,trns,tknChange,flwChange) <- userInput
  let config = defaultPetriTask1Config{
                         basicTask1= 
                             defaultPetriBasicConfig{places = pls, transitions = trns}
                         , tokenChangeOverall = tknChange
                         , flowChangeOverall = flwChange}
  let ct = checkTask1Config config
  let c  = checkBasicConfig (basicTask1 config)
  if isNothing c
  then if isNothing ct 
       then do
         _ <- matchToMath True config
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
  putStr "TokenChange Overall: "
  tknCh <- getLine
  putStr "FlowChange Overall: "
  flwCh <- getLine
  return (read pls, read trns, read tknCh, read flwCh)
