module Main (main) where

import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types
  (BasicConfig(..),ChangeConfig(..),defaultMathConfig,MathConfig(..),Change)
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG              (B,renderSVG)
import Diagrams.Prelude                  (Diagram,mkWidth)
import System.IO
import Text.LaTeX                        (LaTeX,renderFile)
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  pPrint $ defaultMathConfig
  (pls,trns,tknChange,flwChange,sw) <- userInput
  let config = defaultMathConfig{
                         basicTask = 
                           (basicTask defaultMathConfig){places = pls, transitions = trns}
                         , changeTask =
                           (changeTask defaultMathConfig){ tokenChangeOverall = tknChange
                                                   , flowChangeOverall = flwChange}
                         }
  let c = checkConfig config
  let switch 
        | sw == "b" = False
        | otherwise = True
  if isNothing c 
     then do
       (dia,tex,falseNets) <- matchToMath switch config
       renderSVG "app/change0.svg" (mkWidth 200) dia
       renderFile "app/change0.tex" tex
       case falseNets of
         Right falseTex -> parseChangeTex 1 falseTex
         Left falseDia  -> parseChangeDia 1 falseDia
  else
    print c 

userInput :: IO (Int,Int,Int,Int,String)
userInput = do   
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  putStr "TokenChange Overall: "
  tknCh <- getLine
  putStr "FlowChange Overall: "
  flwCh <- getLine
  putStr "Which Tasktype would you like to get?(a: Math to Net, b: Net to Math)"
  sw <- getLine
  return (read pls, read trns, read tknCh, read flwCh,sw)
  
parseChangeDia :: Int -> [(Diagram B, Change)] -> IO()
parseChangeDia _ []                = print "no more Nets"
parseChangeDia i ((dia,change):rs) = do
  print change
  renderSVG ("app/change"++show i++".svg") (mkWidth 200) dia
  parseChangeDia (i+1) rs

parseChangeTex :: Int -> [(LaTeX, Change)] -> IO()
parseChangeTex _ []                = print "no more Nets"
parseChangeTex i ((tex,change):rs) = do 
  print change
  renderFile ("app/change"++show i++".tex") tex
  parseChangeTex (i+1) rs
