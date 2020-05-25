{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types          
  (defaultBasicConfig,BasicConfig(..),defaultFindConflictConfig,FindConflictConfig(..)
  ,defaultPickConflictConfig,PickConflictConfig(..),Conflict,defaultChangeConfig,ChangeConfig(..))
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B,renderSVG)
import Diagrams.Prelude                  (Diagram,mkWidth)
import System.IO
import Text.LaTeX                        (renderFile)
import Text.Pretty.Simple                (pPrint)

main :: IO()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "What type would you like? a: Find a Conflict in a Net, b: Choose the Net with the Conflict"
  sw <- getLine
  if sw == "b" then mainPick else mainFind

mainFind :: IO()
mainFind = do
  pPrint $ defaultFindConflictConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultFindConflictConfig{
                           basicTask = defaultBasicConfig{places = pls, transitions = trns}
                         , changeTask = defaultChangeConfig{ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: FindConflictConfig
  let c = checkChangeConfig (basicTask (config :: FindConflictConfig)) (changeTask (config :: FindConflictConfig))
  if isNothing c
  then do
    (latex,conflDia) <- findConflicts config
    renderFile "app/task2.tex" latex
    parseConflDia 1 conflDia
  else
    print (c :: Maybe String)

mainPick :: IO()
mainPick = do
  pPrint $ defaultPickConflictConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultPickConflictConfig{
                           basicTask = defaultBasicConfig{places = pls, transitions = trns}
                         , changeTask = defaultChangeConfig{ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: PickConflictConfig
  let c = checkChangeConfig (basicTask (config :: PickConflictConfig)) (changeTask (config :: PickConflictConfig))
  if isNothing c
  then do
    (latex,conflDia) <- pickConflicts config
    renderFile "app/task2.tex" latex
    parseConflDia 1 conflDia
  else
    print (c :: Maybe String)
    
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
  return (read pls, read trns,read tknCh, read flwCh)
  
parseConflDia :: Int -> [(Diagram B, Maybe Conflict)] -> IO ()
parseConflDia _ []               = print "no more Nets"
parseConflDia i ((dia,confl):rs) = do
  renderSVG ("app/conflict"++show i++".svg") (mkWidth 400) dia
  print confl
  parseConflDia (i+1) rs