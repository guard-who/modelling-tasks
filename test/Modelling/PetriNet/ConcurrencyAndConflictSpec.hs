{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConcurrencyAndConflictSpec where

import Modelling.PetriNet.ConcurrencyAndConflict (
  checkFindConcurrencyConfig,
  checkFindConflictConfig,
  checkPickConcurrencyConfig,
  checkPickConflictConfig,
  findConcurrency,
  findConflict,
  findTaskInstance,
  parseConcurrency,
  parseConflict,
  petriNetFindConcur,
  petriNetFindConfl,
  petriNetPickConcur,
  petriNetPickConfl,
  pickConcurrency,
  pickConflict,
  pickTaskInstance,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  AlloyConfig (maxInstances, timeout),
  BasicConfig (graphLayout),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  FindConcurrencyConfig (..),
  FindConflictConfig (FindConflictConfig, basicConfig),
  PetriConflict (Conflict),
  PickConcurrencyConfig (..),
  PickConflictConfig (PickConflictConfig, basicConfig),
  defaultAlloyConfig,
  defaultFindConcurrencyConfig,
  defaultFindConflictConfig,
  defaultPickConcurrencyConfig,
  defaultPickConflictConfig,
  )

import Test.Hspec
import Modelling.PetriNet.TestCommon (
  checkConfigs,
  defaultConfigTaskGeneration,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  )

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    checkConfigs checkFindConcurrencyConfig fccs'
  describe "findConcurrency" $ do
    defaultConfigTaskGeneration
      (findConcurrency defaultFindConcurrencyConfig 0)
      0
      checkFindConcurrencyInstance
    testFindConcurrencyConfig fccs
  describe "validPickConcurrencyConfigs" $
    checkConfigs checkPickConcurrencyConfig pccs
  describe "pickConcurrency" $ do
    defaultConfigTaskGeneration
      (pickConcurrency defaultPickConcurrencyConfig 0)
      0
      checkPickConcurrencyInstance
    testPickConcurrencyConfig pccs
  describe "validFindConflictConfigs" $
    checkConfigs checkFindConflictConfig fcfs'
  describe "findConflicts" $ do
    defaultConfigTaskGeneration
      (findConflict defaultFindConflictConfig 0)
      0
      checkFindConflictInstance
    testFindConflictConfig fcfs
  describe "validPickConflictConfigs" $
    checkConfigs checkPickConflictConfig pcfs
  describe "pickConflicts" $ do
    defaultConfigTaskGeneration
      (pickConflict defaultPickConflictConfig 0)
      0
      checkPickConflictInstance
    testPickConflictConfig pcfs
  where
    fccs' = validFindConcurrencyConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fccs  = validAdvConfigs >>= validFindConcurrencyConfigs vcfs
    pccs  = validPickConcurrencyConfigs vcps
    fcfs' = validFindConflictConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcfs  = validAdvConfigs >>= validFindConflictConfigs vcfs
    pcfs  = validPickConflictConfigs vcps
    vcfs  = validConfigsForFind 0 6
    vcps  = validConfigsForPick 0 6

checkFindConcurrencyInstance :: (a, Concurrent String) -> Bool
checkFindConcurrencyInstance = isValidConcurrency . snd

checkFindConflictInstance :: (a, Conflict) -> Bool
checkFindConflictInstance = isValidConflict . snd

checkPickConcurrencyInstance :: [(a, Maybe (Concurrent String))] -> Bool
checkPickConcurrencyInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

checkPickConflictInstance :: [(a, Maybe Conflict)] -> Bool
checkPickConflictInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (\i -> findTaskInstance parseConcurrency i . graphLayout . bc)
  checkFindConcurrencyInstance
  where
    bc :: FindConcurrencyConfig -> BasicConfig
    bc = basicConfig

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConfl
  (\i -> findTaskInstance parseConflict i . graphLayout . bc)
  checkFindConflictInstance
  where
    bc :: FindConflictConfig -> BasicConfig
    bc = basicConfig

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (\i -> pickTaskInstance parseConcurrency i . graphLayout . bc)
  checkPickConcurrencyInstance
  where
    bc :: PickConcurrencyConfig -> BasicConfig
    bc = basicConfig

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConfl
  (\i -> pickTaskInstance parseConflict i . graphLayout . bc)
  checkPickConflictInstance
  where
    bc :: PickConflictConfig -> BasicConfig
    bc = basicConfig

validFindConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConcurrencyConfig]
validFindConcurrencyConfigs cs aconfig =
  uncurry (`FindConcurrencyConfig` aconfig)
    <$> cs
    <*> pure alloyTestConfig

alloyTestConfig :: AlloyConfig
alloyTestConfig = defaultAlloyConfig {
  maxInstances = Nothing,
  timeout = Just 500000
  }

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs aconfig = do
  unique <- [Nothing, Just True, Just False]
  ($ unique) . uncurry (`FindConflictConfig` aconfig)
    <$> cs
    <*> pure alloyTestConfig

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs =
  uncurry PickConcurrencyConfig <$> cs <*> pure alloyTestConfig

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs = do
  unique <- [Nothing, Just True, Just False]
  ($ unique) . uncurry PickConflictConfig <$> cs <*> pure alloyTestConfig

isValidConcurrency :: Concurrent String -> Bool
isValidConcurrency c@(Concurrent (t1, t2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | otherwise                            = error $ show c

isValidConflict :: Conflict -> Bool
isValidConflict c@(Conflict (t1, t2) ps)
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, all isValidPlace ps = True
  | otherwise                                          = error $ show c
  where
    isValidPlace ('s':_) = True
    isValidPlace _       = False
