{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConflictSpec where

import Modelling.PetriNet.Conflict (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflict,
  parseConflict,
  petriNetFindConfl,
  petriNetPickConfl,
  pickConflict,
  )

import Modelling.PetriNet.BasicNetFunctions (
  checkConflictConfig,
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
  FindConflictConfig (FindConflictConfig, alloyConfig),
  PetriConflict (Conflict),
  PetriConflict' (PetriConflict'),
  PickConflictConfig (PickConflictConfig, alloyConfig),
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
  )
import Settings                         (configDepth)

import Control.Lens.Lens                ((??))
import Data.Maybe                       (isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConflictConfigs" $
    checkConfigs checkFindConflictConfig fcfs'
  describe "findConflicts" $ do
    defaultConfigTaskGeneration
      (findConflict defaultFindConflictConfig {
          alloyConfig = firstInstanceConfig
          } 0)
      0
      checkFindConflictInstance
    testFindConflictConfig fcfs
  describe "validPickConflictConfigs" $
    checkConfigs checkPickConflictConfig pcfs
  describe "pickConflicts" $ do
    defaultConfigTaskGeneration
      (pickConflict defaultPickConflictConfig {
          alloyConfig = firstInstanceConfig
          } 0)
      0
      checkPickConflictInstance
    testPickConflictConfig pcfs
  where
    fcfs' = validFindConflictConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcfs  = validAdvConfigs >>= validFindConflictConfigs vcfs
    pcfs  = validPickConflictConfigs vcps
    vcfs  = validConfigsForFind 0 configDepth
    vcps  = validConfigsForPick 0 configDepth

checkFindConflictInstance :: (a, PetriConflict' String) -> Bool
checkFindConflictInstance = isValidConflict . snd

checkPickConflictInstance :: [(a, Maybe (PetriConflict' String))] -> Bool
checkPickConflictInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConfl
  (findTaskInstance parseConflict)
  checkFindConflictInstance

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConfl
  (pickTaskInstance parseConflict)
  checkPickConflictInstance

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs aconfig = do
  (bc, ch) <- cs
  FindConflictConfig bc aconfig ch
    <$> validConflictConfigs bc
    <*> pure False
    <*> [Nothing, Just True, Just False]
    <*> pure alloyTestConfig

validConflictConfigs :: BasicConfig -> [ConflictConfig]
validConflictConfigs bc = filter (isNothing . checkConflictConfig bc) $ do
  precon <- [Nothing, Just False, Just True]
  [ ConflictConfig precon distr Nothing False False
    | distr <- [Nothing, Just False]]
    ++ [ ConflictConfig precon (Just True) distrPrecon distrConfl distrConcur
       | distrPrecon <- [Nothing, Just False, Just True]
       , distrConfl  <- [False, True]
       , let distrConcur = not distrConfl]

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs = do
  (bc, ch) <- cs
  PickConflictConfig bc ch
    <$> validConflictConfigs bc
    <*> pure False
    <*> [False, True]
    <*> [Nothing, Just True, Just False]
    ?? False
    ?? alloyTestConfig

isValidConflict :: PetriConflict' String -> Bool
isValidConflict c@(PetriConflict' (Conflict (t1, t2) ps))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, all isValidPlace ps = True
  | otherwise                                          = error $ show c
  where
    isValidPlace ('s':_) = True
    isValidPlace _       = False
