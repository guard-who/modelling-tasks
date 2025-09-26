{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.ConflictSpec where

import qualified Modelling.PetriNet.Types         as Find (
  FindConflictConfig (alloyConfig),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  PickConflictConfig (alloyConfig),
  )

import Modelling.PetriNet.Conflict (
  checkConflictConfig,
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflict,
  parseConflict,
  petriNetFindConflict,
  petriNetPickConflict,
  pickConflict,
  )

import Modelling.PetriNet.Find (
  findTaskInstance,
  )
import Modelling.PetriNet.Pick (
  pickTaskInstance,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  BasicConfig,
  ChangeConfig,
  ConflictConfig (ConflictConfig),
  FindConflictConfig (FindConflictConfig),
  PetriConflict (Conflict),
  PetriConflict' (PetriConflict'),
  PickConflictConfig (PickConflictConfig),
  SimplePetriLike,
  defaultFindConflictConfig,
  defaultPickConflictConfig,
  )

import Modelling.PetriNet.TestCommon (
  alloyTestConfig,
  checkConfigs,
  defaultConfigTaskGeneration,
  firstInstanceConfig,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  validGraphConfig,
  )
import Settings                         (configDepth, needsTuning)

import Data.Maybe                       (isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConflictConfigs" $
    checkConfigs checkFindConflictConfig findConfigs'
  describe "findConflicts" $ do
    defaultConfigTaskGeneration
      (findConflict defaultFindConflictConfig {
          Find.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkFindConflictInstance @(SimplePetriLike _)
    needsTuning $
      testFindConflictConfig findConfigs
  describe "validPickConflictConfigs" $
    checkConfigs checkPickConflictConfig pickConfigs
  describe "pickConflicts" $ do
    defaultConfigTaskGeneration
      (pickConflict defaultPickConflictConfig {
          Pick.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkPickConflictInstance @(SimplePetriLike _)
    needsTuning $
      testPickConflictConfig pickConfigs
  where
    findConfigs' = validFindConflictConfigs
      validFinds
      (AdvConfig Nothing Nothing Nothing)
    findConfigs = validAdvConfigs >>= validFindConflictConfigs validFinds
    pickConfigs = validPickConflictConfigs validPicks
    validFinds = validConfigsForFind 0 configDepth
    validPicks = validConfigsForPick 0 configDepth

checkFindConflictInstance :: (a, PetriConflict' String) -> Bool
checkFindConflictInstance = isValidConflict . snd

checkPickConflictInstance :: [(a, Maybe (PetriConflict' String))] -> Bool
checkPickConflictInstance = f . map snd
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConflict
  (findTaskInstance parseConflict)
  $ checkFindConflictInstance @(SimplePetriLike _)

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConflict
  (pickTaskInstance parseConflict)
  $ checkPickConflictInstance @(SimplePetriLike _)

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs advancedConfig = [
  FindConflictConfig
    bc
    advancedConfig
    ch
    validConflictConfig
    validGraphConfig
    False
    uniqueConflictPlace
    alloyTestConfig
    Nothing |
      (bc, ch) <- cs,
      validConflictConfig <- validConflictConfigs bc,
      uniqueConflictPlace <- [Nothing, Just True, Just False]
  ]

validConflictConfigs :: BasicConfig -> [ConflictConfig]
validConflictConfigs bc = filter (isNothing . checkConflictConfig bc) $ do
  precondition <- [Nothing, Just False, Just True]
  [ ConflictConfig precondition distractors Nothing False False
    | distractors <- [Nothing, Just False]]
    ++ [ ConflictConfig
           precondition
           (Just True)
           distractorsPrecondition
           distractorsConflict
           distractorsConcurrent
       | distractorsPrecondition <- [Nothing, Just False, Just True]
       , distractorsConflict <- [False, True]
       , let distractorsConcurrent = not distractorsConflict]

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs = [
  PickConflictConfig
    bc
    ch
    validConflictConfig
    validGraphConfig
    False
    prohibitSourceTransitions
    uniqueConflictPlace
    False
    alloyTestConfig
    Nothing |
      (bc, ch) <- cs,
      validConflictConfig <- validConflictConfigs bc,
      prohibitSourceTransitions <- [False, True],
      uniqueConflictPlace <- [Nothing, Just True, Just False]
  ]

isValidConflict :: PetriConflict' String -> Bool
isValidConflict c@(PetriConflict' (Conflict (t1, t2) ps))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, all isValidPlace ps = True
  | otherwise                                          = error $ show c
  where
    isValidPlace ('s':_) = True
    isValidPlace _       = False
