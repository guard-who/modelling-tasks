{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Common                           (forceErrors, printNetAndInfo)
import Modelling.PetriNet.Conflicts (
  findConflicts,
  findConflictsTask,
  pickConflicts,
  pickConflictsTask,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkBasicConfig,
  checkCConfig,
  checkChangeConfig,
  instanceInput
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), ChangeConfig(..), FindConflictConfig(..),
  PickConflictConfig(..),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Maybe                        (isNothing)
import Maybes                            (firstJusts)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
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
mainFind i = forceErrors $ do
  pPrint defaultFindConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
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
    conflDia <- findConflicts i config
    lift $ putStrLn findConflictsTask
    lift $ printNetAndInfo "" conflDia
  else
    lift $ print c

mainPick :: Int -> IO()
mainPick i = forceErrors $ do
  pPrint defaultPickConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
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
    conflDias <- pickConflicts i config
    lift $ putStrLn pickConflictsTask
    lift $ uncurry printNetAndInfo `mapM_` zip ["0", "1"] conflDias
  else
    lift $ print c
    
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
