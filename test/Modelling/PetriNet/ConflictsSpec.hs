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
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  )

import Control.Monad.Trans.Except       (runExceptT)
import Data.Either                      (isRight)
import Test.Hspec

spec :: Spec
spec = do
  describe "validFindConflictConfigs" $
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkFindConflictConfig <$> fcs')
      `shouldBe` []
  describe "findConflicts" $ do
    context "using its default config" $
      it "generates everything required to create the task" $ do
        diaConfl <- runExceptT $ findConflicts 0 defaultFindConflictConfig
        print (snd <$> diaConfl)
        return (isRight diaConfl) `shouldReturn` True
    context "using randomly chosen configs"
      $ testFindConflictConfig fcs
  describe "validPickConflictConfigs" $
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkPickConflictConfig <$> pcs)
      `shouldBe` []
  describe "pickConflicts" $ do
    context "using its default config" $
      it "generates everything required to create the task" $ do
        diaConfls <- runExceptT $ pickConflicts 0 defaultPickConflictConfig
        print (map snd <$> diaConfls)
        return (isRight diaConfls) `shouldReturn` True
    context "using randomly chosen configs"
      $ testPickConflictConfig pcs
  where
    fcs' = validFindConflictConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcs  = validAdvConfigs >>= validFindConflictConfigs vcfs
    pcs  = validPickConflictConfigs vcps
    vcfs = validConfigsForFind 0 6
    vcps = validConfigsForPick 0 6

testFindConflictConfig :: [FindConflictConfig] -> Spec
testFindConflictConfig = testTaskGeneration
  petriNetFindConfl
  (\i -> findConflictsTaskInstance i . graphLayout . bc)
  (f . snd)
  where
    bc :: FindConflictConfig -> BasicConfig
    bc = basicConfig
    f Nothing  = False
    f (Just c) = isValidConflict c

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig = testTaskGeneration
  petriNetPickConfl
  (\i -> pickConflictsTaskInstance i . graphLayout . bc)
  (f . fmap snd)
  where
    bc :: PickConflictConfig -> BasicConfig
    bc = basicConfig
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

isValidConflict :: Conflict -> Bool
isValidConflict c@(Conflict (t1, t2) p)
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, ('s':_) <- p = True
  | otherwise                                          = error $ show c

validFindConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindConflictConfig]
validFindConflictConfigs cs aconfig =
  uncurry (`FindConflictConfig` aconfig) <$> cs

validPickConflictConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickConflictConfig]
validPickConflictConfigs cs =
  uncurry PickConflictConfig <$> cs
