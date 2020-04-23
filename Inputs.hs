module Inputs where 

import Text.Read
import PetriParser
import PetriDiagram        (renderNet)
import AuxFunctions
import Types
import PetriAlloy
import Language.Alloy.Call

userInput :: IO()
userInput = do
  putStr "Anzahl der Stellen: "
  pls <- getLine
  putStr "Anzahl der Transitionen: "
  trns <- getLine
  let inp = defaultInput{ places = read pls :: Int , transitions = read trns :: Int}
  let scp = petriScope inp
  list <- getInstances (Just 5) (petriNetRnd inp scp)
  out <- convertPetri (head list)
  case out of 
    Left error -> print error
    Right petri -> renderNet petri