{-# Language DuplicateRecordFields #-}

module Main (main) where

import Common                           (printNetAndInfo, forceErrors)
import Modelling.PetriNet.Concurrency (
  findConcurrency,
  findConcurrencyTask,
  pickConcurrency,
  pickConcurrencyTask,
  )
import Modelling.PetriNet.BasicNetFunctions(
  checkBasicConfig,
  checkCConfig,
  checkChangeConfig,
  instanceInput,
  )
import Modelling.PetriNet.Types (
  BasicConfig (..), ChangeConfig (..), FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  defaultFindConcurrencyConfig, defaultPickConcurrencyConfig,
  )

import Control.Monad.Trans.Class        (lift)
import Data.Maybe                        (isNothing)
import Maybes                            (firstJusts)
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
  let c = firstJusts 
        [ checkBasicConfig (basicConfig (config :: FindConcurrencyConfig))
        , checkChangeConfig (basicConfig (config :: FindConcurrencyConfig)) (changeConfig (config :: FindConcurrencyConfig))
        , checkCConfig (basicConfig (config ::FindConcurrencyConfig))
        ]
  if isNothing c
  then do
    conc <- findConcurrency i config
    lift $ putStrLn findConcurrencyTask
    lift $ printNetAndInfo "" conc
  else
    lift $ print (c :: Maybe String)
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
  let c = firstJusts 
        [ checkBasicConfig (basicConfig (config :: PickConcurrencyConfig))
        , checkChangeConfig (basicConfig (config :: PickConcurrencyConfig)) (changeConfig (config :: PickConcurrencyConfig))
        ]
  if isNothing c
  then do
    concs <- pickConcurrency i config
    lift $ putStrLn pickConcurrencyTask
    lift $ uncurry printNetAndInfo `mapM_` zip (show <$> [1 :: Integer ..]) concs
  else
    lift $ print (c :: Maybe String)
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
