module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Common (
  forceErrors,
  instanceInput,
  withLang,
  )
import Modelling.PetriNet.MatchToMath (
  MathConfig (..),
  checkMathConfig,
  defaultMathConfig,
  graphToMath,
  graphToMathTask,
  mathToGraph,
  mathToGraphTask,
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  )

import Control.Monad                    (when)
import Control.OutputCapable.Blocks     (Language (English))
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (throwE)
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
  maybe (return ()) throwE $ checkMathConfig config
  let switch
        | sw == "b" = False
        | otherwise = True
  i <- lift instanceInput
  when (i < 0) $ error "There is no negative index"
  lift $ if switch
    then do
    inst <- mathToGraph config 0 i
    mathToGraphTask "fromMath-" inst `withLang` English
    print inst
    else do
    inst <- graphToMath config 0 i
    graphToMathTask "toMathh-" inst `withLang` English
    print inst

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
