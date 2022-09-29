{-# Language DuplicateRecordFields #-}

module Main (main) where

import Common (
  forceErrors,
  instanceInput,
  )
import Modelling.PetriNet.Concurrency (
  checkPickConcurrencyConfig,
  checkFindConcurrencyConfig,
  findConcurrencyGenerate,
  findConcurrencyTask,
  pickConcurrencyGenerate,
  pickConcurrencyTask,
  )
import Modelling.PetriNet.Types (
  BasicConfig (..), ChangeConfig (..), FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  defaultFindConcurrencyConfig, defaultPickConcurrencyConfig,
  )

import Control.Monad.Output             (LangM' (withLang), Language (English))
import Control.Monad.Trans.Class        (lift)
import Data.Maybe                        (isNothing)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "What type would you like? a: Find a concurrency in a Net, b: Choose the Net with the concurrency"
  sw <- getLine
  i <- instanceInput
  if i >= 0 
  then if sw == "b" then mainPick i else mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  lift $ pPrint defaultFindConcurrencyConfig
  (pls,trns,tknChange,flwChange) <- lift userInput
  let config = defaultFindConcurrencyConfig {
        basicConfig = (bc defaultFindConcurrencyConfig) {
            places = pls,
            transitions = trns
            },
        changeConfig = (cc defaultFindConcurrencyConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: FindConcurrencyConfig
  let c = checkFindConcurrencyConfig config
  if isNothing c
  then do
    t <- findConcurrencyGenerate config 0 i
    lift . (`withLang` English) $ findConcurrencyTask "" t
    lift $ print t
  else
    lift $ print c
  where
    bc :: FindConcurrencyConfig -> BasicConfig
    bc = basicConfig
    cc :: FindConcurrencyConfig -> ChangeConfig
    cc = changeConfig
    
mainPick :: Int -> IO ()
mainPick i = forceErrors $ do
  lift $ pPrint defaultPickConcurrencyConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultPickConcurrencyConfig {
        basicConfig = (bc defaultPickConcurrencyConfig) {
            places = pls,
            transitions = trns
            },
        changeConfig = (cc defaultPickConcurrencyConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: PickConcurrencyConfig
  let c = checkPickConcurrencyConfig config
  if isNothing c
  then do
    t <- pickConcurrencyGenerate config 0 i
    lift . (`withLang` English) $ pickConcurrencyTask "" t
    lift $ print t
  else
    lift $ print c
  where
    bc :: PickConcurrencyConfig -> BasicConfig
    bc = basicConfig
    cc :: PickConcurrencyConfig -> ChangeConfig
    cc = changeConfig

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
