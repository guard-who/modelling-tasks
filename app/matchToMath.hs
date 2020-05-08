module Main where

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)

main :: IO ()
main = do
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
