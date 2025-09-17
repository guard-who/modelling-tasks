{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.ConcurrencySpec where

import qualified Modelling.PetriNet.Types         as Find (
  FindConcurrencyConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  PickConcurrencyConfig (..),
  )

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
  AdvConfig (..),
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  SimplePetriLike,
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
  validGraphConfig,
  )
import Settings                         (configDepth, needsTuning)

import Control.Lens.Lens                ((??))
import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    checkConfigs checkFindConcurrencyConfig findConfigs'
  describe "findConcurrency" $ do
    defaultConfigTaskGeneration
      (findConcurrency defaultFindConcurrencyConfig {
          Find.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkFindConcurrencyInstance @(SimplePetriLike _)
    needsTuning $
      testFindConcurrencyConfig findConfigs
  describe "validPickConcurrencyConfigs" $
    checkConfigs checkPickConcurrencyConfig pickConfigs
  describe "pickConcurrency" $ do
    defaultConfigTaskGeneration
      (pickConcurrency defaultPickConcurrencyConfig {
          Pick.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkPickConcurrencyInstance @(SimplePetriLike _)
    needsTuning $
      testPickConcurrencyConfig pickConfigs
  where
    findConfigs' = validFindConcurrencyConfigs
      validFinds
      (AdvConfig Nothing Nothing (Just False))
    findConfigs = validAdvConfigs >>= validFindConcurrencyConfigs validFinds
    pickConfigs = validPickConcurrencyConfigs validPicks
    validFinds = validConfigsForFind 0 configDepth
    validPicks = validConfigsForPick 0 configDepth

checkFindConcurrencyInstance :: (a, Concurrent String) -> Bool
checkFindConcurrencyInstance = isValidConcurrency . snd

checkPickConcurrencyInstance :: [(a, Maybe (Concurrent String))] -> Bool
checkPickConcurrencyInstance = f . map snd
  where
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (findTaskInstance parseConcurrency)
  $ checkFindConcurrencyInstance @(SimplePetriLike _)

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (pickTaskInstance parseConcurrency)
  $ checkPickConcurrencyInstance @(SimplePetriLike _)

validFindConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConcurrencyConfig]
validFindConcurrencyConfigs cs advancedConfig =
 filter
 (\FindConcurrencyConfig{basicConfig = BasicConfig{..}, advConfig = AdvConfig{..}}
  -> presenceOfSourceTransitions == Just False || atLeastActive == 2)
 (
  uncurry (`FindConcurrencyConfig` advancedConfig)
    <$> cs
    ?? validGraphConfig
    ?? False
    ?? alloyTestConfig
    ?? Nothing
 )

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs = [
  PickConcurrencyConfig
    basic
    change
    validGraphConfig
    False
    printSolution
    False
    alloyTestConfig
    Nothing |
      (basic,change) <- cs,
      printSolution <- [False, True]
    ]

isValidConcurrency :: Concurrent String -> Bool
isValidConcurrency c@(Concurrent (t1, t2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | otherwise                            = error $ show c
