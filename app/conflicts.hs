{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Modelling.PetriNet.Conflicts
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.LaTeX          (diagramTex)
import Modelling.PetriNet.Types          
  (BasicConfig(..),defaultFindConflictConfig,FindConflictConfig(..)
  ,defaultPickConflictConfig,PickConflictConfig(..),Conflict,ChangeConfig(..))
import Data.List                         (isInfixOf)
import Data.Maybe                        (isNothing)
import Diagrams.Backend.Rasterific       (renderPdf,B)
import Diagrams.Prelude                  (Diagram,mkWidth)
import Maybes                            (firstJusts)
import System.IO
import Text.LaTeX                        (renderFile)
import Text.Pretty.Simple                (pPrint)

main :: IO()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "What type would you like? a: Find a Conflict in a Net, b: Choose the Net with the Conflict"
  sw <- getLine
  i <- instanceInput
  if i >= 0 
  then if sw == "b" then mainPick i else mainFind i
  else print "There is no negative index"

mainFind ::Int -> IO()
mainFind i = do
  pPrint defaultFindConflictConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultFindConflictConfig{
                           basicTask = (basicTask (defaultFindConflictConfig :: FindConflictConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultFindConflictConfig :: FindConflictConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: FindConflictConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: FindConflictConfig))
        , checkChangeConfig (basicTask (config :: FindConflictConfig)) (changeTask (config :: FindConflictConfig))
        , checkCConfig (basicTask (config :: FindConflictConfig)) 
        ]
  if isNothing c
  then do
    (latex,conflDia) <- findConflicts i config
    renderFile "app/findConflictTask.tex" latex
    conflicts <- parseConflDia 0 conflDia
    let texN = diagramTex conflicts (length conflicts - 1) latex
    out <- renderFile "app/findConflict.tex" texN >> renderPdfFile "app/findConflict.tex"
    if "Output written on" `isInfixOf` out
    then print "PDF succesfully generated"
    else print "Error upon generating the PDF-File"
  else
    print (c :: Maybe String)

mainPick :: Int -> IO()
mainPick i = do
  pPrint defaultPickConflictConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultPickConflictConfig{
                           basicTask = (basicTask (defaultPickConflictConfig :: PickConflictConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultPickConflictConfig :: PickConflictConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: PickConflictConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: PickConflictConfig))
        , checkChangeConfig (basicTask (config :: PickConflictConfig)) (changeTask (config :: PickConflictConfig))
        ]
  if isNothing c
  then do
    (latex,conflDia) <- pickConflicts i config
    renderFile "app/pickConflictTask.tex" latex
    conflicts <- parseConflDia 0 conflDia
    let texN = diagramTex conflicts (length conflicts - 1) latex
    out <- renderFile "app/pickConflict.tex" texN >> renderPdfFile "app/pickConflict.tex"
    if "Output written on" `isInfixOf` out
    then print "PDF succesfully generated"
    else print "Error upon generating the PDF-File"
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
  
parseConflDia :: Int -> [(Diagram B, Maybe Conflict)] -> IO [Maybe Conflict]
parseConflDia _ []               = return []
parseConflDia i ((dia,confl):rs) = do
  renderPdf 1000 1000 ("app/"++show i++".pdf") (mkWidth 800) dia
  print confl
  rest <- parseConflDia (i+1) rs
  return $ confl : rest