module Main (main) where 

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types          
  (defaultBasicConfig,BasicConfig(..),defaultConflictConfig,ConflictConfig(..),Conflict)
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B,renderSVG)
import Diagrams.Prelude                  (Diagram,mkWidth)
import System.IO
import Text.LaTeX                        (renderFile)

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  (pls,trns,nets,sw) <- userInput
  let config = defaultConflictConfig{basicTask = defaultBasicConfig{places = pls, transitions = trns}}
  let c = checkBasicConfig (basicTask config)
  let switch 
        | sw == "b" = False
        | otherwise = True
  if isNothing c
  then do 
    (latex,conflDia) <- findConflicts switch nets (basicTask config)
    renderFile ("app/task2.tex") latex
    parseConflDia 1 conflDia
  else
    print c

userInput :: IO (Int,Int,Int,String)
userInput = do   
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  putStr "Number of ConflictNets: "
  nets <- getLine
  putStr "Which tasktype would you like to use? a:Show the occuring conflicts in the Nets, b: Show which Net doesn't have a Conflict \n"
  switch <- getLine
  return (read pls, read trns,read nets,switch)
  
parseConflDia :: Int -> [(Diagram B, Maybe Conflict)] -> IO ()
parseConflDia _ []               = print "no more Nets"
parseConflDia i ((dia,confl):rs) = do
  renderSVG ("app/conflict"++show i++".svg") (mkWidth 200) dia
  print confl
  parseConflDia (i+1) rs