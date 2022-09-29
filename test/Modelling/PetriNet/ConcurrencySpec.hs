{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConcurrencySpec where

import Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig,
  checkPickConcurrencyConfig,
  findConcurrency,
  parseConcurrency,
  petriNetFindConcur,
  petriNetPickConcur,
  pickConcurrency,
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
  Concurrent (Concurrent),
  FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  defaultFindConcurrencyConfig,
  defaultPickConcurrencyConfig,
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
import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    checkConfigs checkFindConcurrencyConfig fccs'
  describe "findConcurrency" $ do
    defaultConfigTaskGeneration
      (findConcurrency defaultFindConcurrencyConfig {
          alloyConfig = firstInstanceConfig
          } 0)
      0
      checkFindConcurrencyInstance
    testFindConcurrencyConfig fccs
  describe "validPickConcurrencyConfigs" $
    checkConfigs checkPickConcurrencyConfig pccs
  describe "pickConcurrency" $ do
    defaultConfigTaskGeneration
      (pickConcurrency defaultPickConcurrencyConfig {
          alloyConfig = firstInstanceConfig
          } 0)
      0
      checkPickConcurrencyInstance
    testPickConcurrencyConfig pccs
  where
    fccs' = validFindConcurrencyConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fccs  = validAdvConfigs >>= validFindConcurrencyConfigs vcfs
    pccs  = validPickConcurrencyConfigs vcps
    vcfs  = validConfigsForFind 0 configDepth
    vcps  = validConfigsForPick 0 configDepth

checkFindConcurrencyInstance :: (a, Concurrent String) -> Bool
checkFindConcurrencyInstance = isValidConcurrency . snd

checkPickConcurrencyInstance :: [(a, Maybe (Concurrent String))] -> Bool
checkPickConcurrencyInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (findTaskInstance parseConcurrency)
  checkFindConcurrencyInstance

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (pickTaskInstance parseConcurrency)
  checkPickConcurrencyInstance

validFindConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConcurrencyConfig]
validFindConcurrencyConfigs cs aconfig =
  uncurry (`FindConcurrencyConfig` aconfig)
    <$> cs
    ?? False
    ?? alloyTestConfig

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs = uncurry PickConcurrencyConfig
  <$> cs
  <*> pure False
  <*> [False, True]
  ?? False
  ?? alloyTestConfig

isValidConcurrency :: Concurrent String -> Bool
isValidConcurrency c@(Concurrent (t1, t2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | otherwise                            = error $ show c
