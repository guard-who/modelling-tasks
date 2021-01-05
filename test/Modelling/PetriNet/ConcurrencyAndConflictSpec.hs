{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConcurrencyAndConflictSpec where

import Modelling.PetriNet.ConcurrencyAndConflict (
  checkFindConcurrencyConfig,
  checkFindConflictConfig,
  checkPickConcurrencyConfig,
  checkPickConflictConfig,
  findConcurrency,
  findConflicts,
  findTaskInstance,
  pickConcurrency,
  pickConflicts,
  pickTaskInstance,
  )
import Modelling.PetriNet.Parser        (
  parseConcurrency,
  parseConflict,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  BasicConfig (graphLayout),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  FindConcurrencyConfig (..),
  FindConflictConfig (FindConflictConfig, basicConfig),
  PetriConflict (Conflict),
  PickConcurrencyConfig (..),
  PickConflictConfig (PickConflictConfig, basicConfig),
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
import Modelling.PetriNet.Alloy (
  petriNetFindConcur, petriNetFindConfl, petriNetPickConcur, petriNetPickConfl,
  )

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    checkConfigs checkFindConcurrencyConfig fccs'
  describe "findConcurrency" $ do
    defaultConfigTaskGeneration
      (findConcurrency 0 defaultFindConcurrencyConfig)
      checkFindConcurrencyInstance
    testFindConcurrencyConfig fccs
  describe "validPickConcurrencyConfigs" $
    checkConfigs checkPickConcurrencyConfig pccs
  describe "pickConcurrency" $ do
    defaultConfigTaskGeneration
      (pickConcurrency 0 defaultPickConcurrencyConfig)
      checkPickConcurrencyInstance
    testPickConcurrencyConfig pccs
  describe "validFindConflictConfigs" $
    checkConfigs checkFindConflictConfig fcfs'
  describe "findConflicts" $ do
    defaultConfigTaskGeneration
      (findConflicts 0 defaultFindConflictConfig)
      checkFindConflictInstance
    testFindConflictConfig fcfs
  describe "validPickConflictConfigs" $
    checkConfigs checkPickConflictConfig pcfs
  describe "pickConflicts" $ do
    defaultConfigTaskGeneration
      (pickConflicts 0 defaultPickConflictConfig)
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

checkFindConcurrencyInstance :: (a, Maybe (Concurrent String)) -> Bool
checkFindConcurrencyInstance = f . snd
  where
    f Nothing  = False
    f (Just c) = isValidConcurrency c

checkFindConflictInstance :: (a, Maybe Conflict) -> Bool
checkFindConflictInstance = f . snd
  where
    f Nothing  = False
    f (Just c) = isValidConflict c

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
  uncurry (`FindConcurrencyConfig` aconfig) <$> cs

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs aconfig = do
  unique <- [Nothing, Just True, Just False]
  ($unique) . uncurry (`FindConflictConfig` aconfig) <$> cs

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs =
  uncurry PickConcurrencyConfig <$> cs

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs = do
  unique <- [Nothing, Just True, Just False]
  ($unique) . uncurry PickConflictConfig <$> cs

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
