module Main (main) where

import Modelling.PetriNet.BasicNetFunctions (instanceInput,renderPdfFile)

import Modelling.PetriNet.MatchToMath
import Modelling.PetriNet.Types
  (BasicConfig(..),ChangeConfig(..),defaultMathConfig,MathConfig(..),Change)
import Modelling.PetriNet.LaTeX          (diagramTex,texTex)

import Data.List                         (isInfixOf)
import Data.Maybe                        (isNothing)
import Diagrams.Prelude                  (Diagram,mkWidth)
import Diagrams.Backend.Rasterific       (renderPdf,B)
import System.IO
import Text.LaTeX                        (LaTeX,renderFile)
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  pPrint defaultMathConfig
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
       i <- instanceInput
       if i < 0
       then error "There is no negative index"
       else do
         (dia,tex,falseNets) <- matchToMath i switch config
         renderPdf 300 300 "app/0.pdf" (mkWidth 300) dia
         case falseNets of
           Right falseTex -> do
             parseChangeTex 1 falseTex
             let (texList,changes) = unzip falseTex
             let texN = texTex changes texList tex 
             out <- renderFile "app/mathB.tex" texN >> renderPdfFile "app/mathB.tex"
             if "Output written on" `isInfixOf` out
             then print "PDF succesfully generated"
             else print "Error upon generating the PDF-File"
           Left falseDia  -> do
             changes <- parseChangeDia 1 falseDia
             renderFile "app/mathTask.tex" tex
             let texN = diagramTex changes (length changes) tex 
             out <- renderFile "app/math.tex" texN >> renderPdfFile "app/math.tex"
             if "Output written on" `isInfixOf` out
             then print "PDF succesfully generated"
             else print "Error upon generating the PDF-File"
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
  putStr "Which Tasktype would you like to get?(a: Math to Net, b: Net to Math): "
  sw <- getLine
  return (read pls, read trns, read tknCh, read flwCh,sw)
  
parseChangeDia :: Int -> [(Diagram B, Change)] -> IO [Change]
parseChangeDia _ []                = return []
parseChangeDia i ((dia,change):rs) = do
  print change
  renderPdf 300 300 ("app/"++show i++".pdf") (mkWidth 300) dia
  rest <- parseChangeDia (i+1) rs
  return $ change : rest

parseChangeTex :: Int -> [(LaTeX, Change)] -> IO()
parseChangeTex _ []                = print "no more Nets"
parseChangeTex i ((tex,change):rs) = do 
  print change
  renderFile ("app/mathB"++show i++".tex") tex
  parseChangeTex (i+1) rs
