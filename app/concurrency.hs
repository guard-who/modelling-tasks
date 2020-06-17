{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Modelling.PetriNet.Concurrency
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.LaTeX          (diagramTex)
import Modelling.PetriNet.Types          
  (BasicConfig(..),defaultFindConcurrencyConfig,FindConcurrencyConfig(..)
  ,defaultPickConcurrencyConfig,PickConcurrencyConfig(..),Concurrent,ChangeConfig(..))
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
  putStr "What type would you like? a: Find a concurrency in a Net, b: Choose the Net with the concurrency"
  sw <- getLine
  i <- instanceInput
  if i >= 0 
  then if sw == "b" then mainPick i else mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO()
mainFind i = do
  pPrint defaultFindConcurrencyConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultFindConcurrencyConfig{
                           basicTask = (basicTask (defaultFindConcurrencyConfig :: FindConcurrencyConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultFindConcurrencyConfig :: FindConcurrencyConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: FindConcurrencyConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: FindConcurrencyConfig))
        , checkChangeConfig (basicTask (config :: FindConcurrencyConfig)) (changeTask (config :: FindConcurrencyConfig))
        , checkCConfig (basicTask (config ::FindConcurrencyConfig)) 
        ]
  if isNothing c
  then do
    (latex,concDia) <- findConcurrency i config
    renderFile "app/FindConcurrentTask.tex" latex
    concurrents <- parseConcDia 0 concDia
    let texN = diagramTex concurrents (length concurrents - 1) latex
    out <- renderFile "app/findConcurrent.tex" texN >> renderPdfFile "app/findConcurrent.tex"
    if "Output written on" `isInfixOf` out
    then print "PDF succesfully generated"
    else print "Error upon generating the PDF-File"
  else
    print (c :: Maybe String)
    
mainPick :: Int -> IO()
mainPick i = do
  pPrint defaultPickConcurrencyConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultPickConcurrencyConfig{
                           basicTask = (basicTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: PickConcurrencyConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: PickConcurrencyConfig))
        , checkChangeConfig (basicTask (config :: PickConcurrencyConfig)) (changeTask (config :: PickConcurrencyConfig)) 
        ]
  if isNothing c
  then do

    (latex,concDia) <- pickConcurrency i config
    renderFile "app/pickConcurrentTask.tex" latex
    concurrents <- parseConcDia 0 concDia
    let texN = diagramTex concurrents (length concurrents - 1) latex
    out <- renderFile "app/pickConcurrent.tex" texN >> renderPdfFile "app/pickConcurrent.tex"
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
  
parseConcDia :: Int -> [(Diagram B, Maybe Concurrent)] -> IO [Maybe Concurrent]
parseConcDia _ []               = return []
parseConcDia i ((dia,conc):rs) = do
  renderPdf 500 500 ("app/concurrency"++show i++".png") (mkWidth 400) dia
  print conc
  rest <- parseConcDia (i+1) rs
  return $ conc : rest