module Inputs where 

import Text.Read
import PetriParser
import PetriDiagram        (renderNet)
import AuxFunctions
import Language.Alloy.Call

userInput :: IO()
userInput = do
  putStr "Anzahl der Stellen: "
  pls <- getLine
  putStr "Anzahl der Transitionen: "
  trns <- getLine
  let plsI = (read pls :: Int)
  let trnsI = (read trns :: Int)
  list <- getInstances (Just 5) (petriNetRnd defaultInput{ places = plsI, transitions = trnsI} )
  out <- convertPetri (head list)
  case out of 
    Left error -> print error
    Right petri -> renderNet petri