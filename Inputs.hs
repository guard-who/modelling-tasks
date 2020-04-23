{-# LANGUAGE NamedFieldPuns #-}

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
  if checkInput inp 
  then do
    out <- runPParser inp scp
    case out of 
      Left error -> print error
      Right petri -> renderNet petri
  else 
    print "invalid Input"
    
checkInput :: Input -> Bool
checkInput Input{places,transitions,tkns,maxTkns,maxWght,activated} = 
  places > 0 && transitions > 0 && tkns > 0 &&
  maxTkns <= tkns &&
  tkns <= places*maxTkns &&
  activated <= transitions &&
  maxWght <= maxTkns 
  