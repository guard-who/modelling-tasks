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

import Modelling.PetriNet.BasicNetFunctions (
  checkConflictConfig,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  BasicConfig (graphLayout, hidePlaceNames, hideTransitionNames, hideWeight1),
  ChangeConfig,
  Concurrent (Concurrent),
  ConflictConfig (ConflictConfig),
  FindConcurrencyConfig (..),
  FindConflictConfig (FindConflictConfig, alloyConfig),
  PetriConflict (Conflict),
  PetriConflict' (PetriConflict'),
  PickConcurrencyConfig (..),
  PickConflictConfig (PickConflictConfig, alloyConfig),
  defaultFindConcurrencyConfig,
  defaultFindConflictConfig,
  defaultPickConcurrencyConfig,
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
import Data.GraphViz                    (GraphvizCommand)
import Data.Maybe                       (isNothing)
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
    fccs' = validFindConcurrencyConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fccs  = validAdvConfigs >>= validFindConcurrencyConfigs vcfs
    pccs  = validPickConcurrencyConfigs vcps
    fcfs' = validFindConflictConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcfs  = validAdvConfigs >>= validFindConflictConfigs vcfs
    pcfs  = validPickConflictConfigs vcps
    vcfs  = validConfigsForFind 0 configDepth
    vcps  = validConfigsForPick 0 configDepth

checkFindConcurrencyInstance :: (a, Concurrent String) -> Bool
checkFindConcurrencyInstance = isValidConcurrency . snd

checkFindConflictInstance :: (a, PetriConflict' String) -> Bool
checkFindConflictInstance = isValidConflict . snd

checkPickConcurrencyInstance :: [(a, Maybe (Concurrent String))] -> Bool
checkPickConcurrencyInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

checkPickConflictInstance :: [(a, Maybe (PetriConflict' String))] -> Bool
checkPickConflictInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

{-|
Beware: calling this function is only safe if the list of ''graphLayout's
is not empty.
-}
addDrawArgs
  :: (a -> BasicConfig)
  -> (Bool -> Bool -> Bool -> GraphvizCommand -> b)
  -> a
  -> b
addDrawArgs f g c = g
  (hidePlaceNames $ f c)
  (hideTransitionNames $ f c)
  (hideWeight1 $ f c)
  (head $ graphLayout $ f c)

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (findTaskInstance parseConcurrency)
  checkFindConcurrencyInstance

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConfl
  (findTaskInstance parseConflict)
  checkFindConflictInstance

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (pickTaskInstance parseConcurrency)
  checkPickConcurrencyInstance

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConfl
  (pickTaskInstance parseConflict)
  checkPickConflictInstance

validFindConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConcurrencyConfig]
validFindConcurrencyConfigs cs aconfig =
  uncurry (`FindConcurrencyConfig` aconfig)
    <$> cs
    ?? False
    ?? alloyTestConfig

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

validPickConcurrencyConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConcurrencyConfig]
validPickConcurrencyConfigs cs = uncurry PickConcurrencyConfig
  <$> cs
  <*> pure False
  <*> [False, True]
  ?? False
  ?? alloyTestConfig

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

isValidConcurrency :: Concurrent String -> Bool
isValidConcurrency c@(Concurrent (t1, t2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | otherwise                            = error $ show c

isValidConflict :: PetriConflict' String -> Bool
isValidConflict c@(PetriConflict' (Conflict (t1, t2) ps))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, all isValidPlace ps = True
  | otherwise                                          = error $ show c
  where
    isValidPlace ('s':_) = True
    isValidPlace _       = False
