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

import Control.Monad.Trans.Except       (runExceptT)
import Test.Hspec
import Modelling.PetriNet.TestCommon (
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  )
import Modelling.PetriNet.Alloy         (petriNetFindConcur, petriNetPickConcur)

spec :: Spec
spec = do
  describe "validFindConcurrencyConfigs" $
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkFindConcurrencyConfig <$> fcs')
      `shouldBe` []
  describe "findConcurrency" $ do
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConc <- runExceptT $ findConcurrency 0 defaultFindConcurrencyConfig
        print (snd diaConc) `shouldReturn` ()
    context "using randomly chosen configs"
      $ testFindConcurrencyConfig fcs
  describe "validPickConcurrencyConfigs" $
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkPickConcurrencyConfig <$> pcs)
      `shouldBe` []
  describe "pickConcurrency" $ do
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConc <- runExceptT $ pickConcurrency 0 defaultPickConcurrencyConfig
        print (map snd diaConc) `shouldReturn` ()
    context "using randomly chosen configs"
      $ testPickConcurrencyConfig pcs
  where
    fcs' = validFindConcurrencyConfigs vcfs (AdvConfig Nothing Nothing Nothing)
    fcs  = validAdvConfigs >>= validFindConcurrencyConfigs vcfs
    pcs  = validPickConcurrencyConfigs vcps
    vcfs = validConfigsForFind 0 6
    vcps = validConfigsForPick 0 6

testFindConcurrencyConfig :: [FindConcurrencyConfig] -> Spec
testFindConcurrencyConfig = testTaskGeneration
  petriNetFindConcur
  (\i -> findConcurrencyTaskInstance i . graphLayout . bc)
  (f . snd)
  where
    bc :: FindConcurrencyConfig -> BasicConfig
    bc = basicConfig
    f Nothing  = False
    f (Just c) = isValidConcurrency c

testPickConcurrencyConfig :: [PickConcurrencyConfig] -> Spec
testPickConcurrencyConfig = testTaskGeneration
  petriNetPickConcur
  (\i -> pickConcurrencyTaskInstance i . graphLayout . bc)
  (f . fmap snd)
  where
    bc :: PickConcurrencyConfig -> BasicConfig
    bc = basicConfig
    f [Just x, Nothing] = isValidConcurrency x
    f _                 = False

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
