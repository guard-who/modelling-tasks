{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConcurrencySpec where

import Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig,
  checkPickConcurrencyConfig,
  findConcurrency,
  findConcurrencyTaskInstance,
  pickConcurrency,
  pickConcurrencyTaskInstance,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  BasicConfig (graphLayout),
  ChangeConfig,
  Concurrent (Concurrent),
  FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  defaultFindConcurrencyConfig,
  defaultPickConcurrencyConfig,
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
import Modelling.PetriNet.Alloy         (petriNetFindConcur, petriNetPickConcur)

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    checkConfigs checkFindConcurrencyConfig fcs'
  describe "findConcurrency" $ do
    defaultConfigTaskGeneration
      (findConcurrency 0 defaultFindConcurrencyConfig)
      checkFindInstance
    testFindConcurrencyConfig fcs
  describe "validPickConcurrencyConfigs" $
    checkConfigs checkPickConcurrencyConfig pcs
  describe "pickConcurrency" $ do
    defaultConfigTaskGeneration
      (pickConcurrency 0 defaultPickConcurrencyConfig)
      checkPickInstance
    testPickConcurrencyConfig pcs
  where
    fcs' = validFindConcurrencyConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcs  = validAdvConfigs >>= validFindConcurrencyConfigs vcfs
    pcs  = validPickConcurrencyConfigs vcps
    vcfs = validConfigsForFind 0 6
    vcps = validConfigsForPick 0 6

checkFindInstance :: (a, Maybe (Concurrent String)) -> Bool
checkFindInstance = f . snd
  where
    f Nothing  = False
    f (Just c) = isValidConcurrency c

checkPickInstance :: [(a, Maybe (Concurrent String))] -> Bool
checkPickInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (\i -> findConcurrencyTaskInstance i . graphLayout . bc)
  checkFindInstance
  where
    bc :: FindConcurrencyConfig -> BasicConfig
    bc = basicConfig

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (\i -> pickConcurrencyTaskInstance i . graphLayout . bc)
  checkPickInstance
  where
    bc :: PickConcurrencyConfig -> BasicConfig
    bc = basicConfig

isValidConcurrency :: Concurrent String -> Bool
isValidConcurrency c@(Concurrent (t1, t2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | otherwise                            = error $ show c

validFindConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConcurrencyConfig]
validFindConcurrencyConfigs cs aconfig =
  uncurry (`FindConcurrencyConfig` aconfig) <$> cs

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs =
  uncurry PickConcurrencyConfig <$> cs
