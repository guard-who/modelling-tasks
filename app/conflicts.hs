{-# Language DuplicateRecordFields #-}

module Main (main) where


import qualified Modelling.PetriNet.Types         as Find (
  FindConflictConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  PickConflictConfig (..),
  )

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Common (
  forceErrors,
  instanceInput,
  withLang,
  )
import Modelling.PetriNet.Conflict (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflictGenerate,
  pickConflictGenerate,
  simpleFindConflictTask,
  simplePickConflictTask,
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), ChangeConfig(..), FindConflictConfig(..),
  PickConflictConfig(..),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Control.OutputCapable.Blocks     (Language (English))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Maybe                       (isNothing)
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

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  pPrint defaultFindConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultFindConflictConfig {
        Find.basicConfig = (Find.basicConfig defaultFindConflictConfig) {
            places = pls,
            transitions = trns
            },
        Find.changeConfig = (Find.changeConfig defaultFindConflictConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: FindConflictConfig
  let c = checkFindConflictConfig config
  lift $
    if isNothing c
    then do
      t <- findConflictGenerate config 0 i
      simpleFindConflictTask "" t `withLang` English
      print t
    else print c

mainPick :: Int -> IO ()
mainPick i = forceErrors $ do
  pPrint defaultPickConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultPickConflictConfig {
        Pick.basicConfig = (Pick.basicConfig defaultPickConflictConfig) {
            places = pls,
            transitions = trns
            },
        Pick.changeConfig = (Pick.changeConfig defaultPickConflictConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: PickConflictConfig
  let c = checkPickConflictConfig config
  lift $
    if isNothing c
    then do
      t <- pickConflictGenerate config 0 i
      simplePickConflictTask "" t `withLang` English
      print t
    else print c

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
