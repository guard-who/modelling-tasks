{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Modelling.PetriNet.Concurrency
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types          
  (BasicConfig(..),defaultFindConcurrencyConfig,FindConcurrencyConfig(..)
  ,defaultPickConcurrencyConfig,PickConcurrencyConfig(..),Concurrent,ChangeConfig(..))
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B,renderSVG)
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
  if sw == "b" then mainPick else mainFind

mainFind :: IO()
mainFind = do
  pPrint $ defaultFindConcurrencyConfig
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
    (latex,concDia) <- findConcurrency config
    renderFile "app/task3.tex" latex
    parseConcDia 1 concDia
  else
    print (c :: Maybe String)
    
mainPick :: IO()
mainPick = do
  pPrint $ defaultPickConcurrencyConfig
  (pls,trns,tknChange,flwChange) <- userInput 
  let config = defaultPickConcurrencyConfig{
                           basicTask = (basicTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: PickConcurrencyConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: PickConcurrencyConfig))
        , checkChangeConfig (basicTask (config :: PickConcurrencyConfig)) (changeTask (config :: PickConcurrencyConfig))
        , checkCConfig (basicTask (config ::PickConcurrencyConfig)) 
        ]
  if isNothing c
  then do
    (latex,concDia) <- pickConcurrency config
    renderFile "app/task3.tex" latex
    parseConcDia 1 concDia
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
  
parseConcDia :: Int -> [(Diagram B, Maybe Concurrent)] -> IO ()
parseConcDia _ []               = print "no more Nets"
parseConcDia i ((dia,conc):rs) = do
  renderSVG ("app/concurrency"++show i++".svg") (mkWidth 400) dia
  print conc
  parseConcDia (i+1) rs