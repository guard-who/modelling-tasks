module Main (main) where

import Common (
  forceErrors,
  instanceInput,
  renderPetriNet,
  )
import Modelling.PetriNet.MatchToMath (
  MathConfig (..),
  checkConfig,
  defaultMathConfig,
  graphToMath,
  matchToMathTask,
  mathToGraph
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  PetriMath (..),
  )

import Control.Monad                    (when)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, except, throwE)
import Data.Bifunctor                   (bimap)
import Image.LaTeX.Render               (
  Formula, SVG,
  alterForHTML, defaultEnv, defaultFormulaOptions, imageForFormula,
  )
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = forceErrors $ do
  lift $ hSetBuffering stdout NoBuffering
  lift $ pPrint defaultMathConfig
  (pls, trns, tknChange, flwChange, sw) <- lift userInput
  let config = defaultMathConfig{
        basicConfig =
            (basicConfig defaultMathConfig) {places = pls, transitions = trns},
        changeConfig =
            (changeConfig defaultMathConfig) {
            tokenChangeOverall = tknChange,
                flowChangeOverall = flwChange}
        }
  maybe (return ()) throwE $ checkConfig config
  let switch 
        | sw == "b" = False
        | otherwise = True
  i <- lift instanceInput
  when (i < 0) $ error "There is no negative index"
  lift $ putStrLn $ matchToMathTask switch
  if switch
    then do
    (dia, math, falseDia) <- mathToGraph config 0 i
    lift $ renderPetriNet "0" dia
    saveMathFiles "0" math
    let writeBoth num (x, y) = do
          renderPetriNet (show num) x
          print y
    lift $ uncurry writeBoth `mapM_` zip [1 :: Integer ..] falseDia
    else do
    (dia, math, falseTex) <- graphToMath config 0 i
    lift $ renderPetriNet "0" dia
    saveMathFiles "0" math
    let writeBoth num (x, y) = do
          saveMathFiles (show num) x
          lift $ print y
    uncurry writeBoth `mapM_` zip [1 :: Integer ..] falseTex

saveMathFiles :: String -> PetriMath Formula -> ExceptT String IO ()
saveMathFiles name m = do
  eformula <- lift $ mapM renderFormula m
  formula  <- except `mapM` eformula
  writeFile' "net" $ netMath formula
  writeFile' "places" $ placesMath formula
  writeFile' "transitions" $ transitionsMath formula
  uncurry writeChange `mapM_` zip [1 :: Integer ..] (tokenChangeMath formula)
  writeFile' "marking" $ initialMarkingMath formula
  where
    writeChange n (x, y) = do
      writeFile' ("in" ++ show n) x
      writeFile' ("out" ++ show n) y
    writeFile' x =
      let y = name ++ x ++ ".svg"
      in lift . (putStrLn ("wrote file " ++ y) >>) . writeFile y

renderFormula :: String -> IO (Either String SVG)
renderFormula = (bimap show alterForHTML <$>)
  . imageForFormula defaultEnv defaultFormulaOptions

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
