{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConflictsSpec (
  spec
  ) where

import Modelling.PetriNet.Alloy         (petriNetFindConfl, petriNetPickConfl)
import Modelling.PetriNet.Conflicts (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflicts,
  findConflictsTaskInstance,
  pickConflicts,
  pickConflictsTaskInstance,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig), BasicConfig (graphLayout), ChangeConfig,
  Conflict,
  FindConflictConfig (FindConflictConfig, basicConfig),
  PetriConflict (Conflict),
  PickConflictConfig (PickConflictConfig, basicConfig),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Modelling.PetriNet.TestCommon (
  checkConfigs,
  defaultConfigTaskGeneration,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConflictConfigs" $
    checkConfigs checkFindConflictConfig fcs'
  describe "findConflicts" $ do
    defaultConfigTaskGeneration
      (findConflicts 0 defaultFindConflictConfig)
      checkFindInstance
    testFindConflictConfig fcs
  describe "validPickConflictConfigs" $
    checkConfigs checkPickConflictConfig pcs
  describe "pickConflicts" $ do
    defaultConfigTaskGeneration
      (pickConflicts 0 defaultPickConflictConfig)
      checkPickInstance
    testPickConflictConfig pcs
  where
    fcs' = validFindConflictConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcs  = validAdvConfigs >>= validFindConflictConfigs vcfs
    pcs  = validPickConflictConfigs vcps
    vcfs = validConfigsForFind 0 6
    vcps = validConfigsForPick 0 6

checkFindInstance :: (a, Maybe Conflict) -> Bool
checkFindInstance = f . snd
  where
    f Nothing  = False
    f (Just c) = isValidConflict c

checkPickInstance :: [(a, Maybe Conflict)] -> Bool
checkPickInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConfl
  (\i -> findConflictsTaskInstance i . graphLayout . bc)
  checkFindInstance
  where
    bc :: FindConflictConfig -> BasicConfig
    bc = basicConfig

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConfl
  (\i -> pickConflictsTaskInstance i . graphLayout . bc)
  checkPickInstance
  where
    bc :: PickConflictConfig -> BasicConfig
    bc = basicConfig

isValidConflict :: Conflict -> Bool
isValidConflict c@(Conflict (t1, t2) ps)
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, all isValidPlace ps = True
  | otherwise                                          = error $ show c
  where
    isValidPlace ('s':_) = True
    isValidPlace _       = False

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs aconfig = do
  unique <- [Nothing, Just True, Just False]
  ($unique) . uncurry (`FindConflictConfig` aconfig) <$> cs

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs = do
  unique <- [Nothing, Just True, Just False]
  ($unique) . uncurry PickConflictConfig <$> cs
