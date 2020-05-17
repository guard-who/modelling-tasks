module Main (main) where 

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B,renderSVG)
import Diagrams.Prelude                  (Diagram,mkWidth)
import System.IO
import Text.LaTeX                        (renderFile)

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  (pls,trns) <- userInput
  let config = defaultPetriBasicConfig{places = pls, transitions = trns}
  let c = checkBasicConfig config
  if isNothing c
  then do 
    (latex,conflDia) <- findConflicts True config
    renderFile ("app/task2.tex") latex
    parseConflDia 1 conflDia
  else
    print c

userInput :: IO (Int,Int)
userInput = do   
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  return (read pls, read trns)
  
parseConflDia :: Int -> [(Diagram B, Maybe Conflict)] -> IO ()
parseConflDia _ []               = print "no more Nets"
parseConflDia i ((dia,confl):rs) = do
  renderSVG ("app/conflict"++show i++".svg") (mkWidth 200) dia
  print confl
  parseConflDia (i+1) rs