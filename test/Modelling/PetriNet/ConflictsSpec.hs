{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.PetriNet.ConflictsSpec (
  spec
  ) where

import Modelling.PetriNet.Conflicts (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflicts,
  findConflictsTaskInstance,
  getAlloyInstances,
  pickConflicts,
  pickConflictsTaskInstance,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig), BasicConfig (..), ChangeConfig (ChangeConfig),
  Conflict,
  FindConflictConfig (FindConflictConfig, basicConfig),
  PetriConflict (Conflict),
  PickConflictConfig (PickConflictConfig, basicConfig),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Control.Monad.Trans.Except       (runExceptT)
import Data.Either                      (isRight)
import Data.GraphViz                    (GraphvizCommand (Neato))
import Language.Alloy.Call (
  CallAlloyConfig (..), defaultCallAlloyConfig,
  )
import System.Random                    (StdGen, mkStdGen, randomR)
import Test.Hspec
import Test.Hspec.QuickCheck            (modifyMaxSuccess)
import Test.QuickCheck                  (Property, (==>), ioProperty, property)
import Modelling.PetriNet.Alloy         (petriNetFindConfl, petriNetPickConfl)

spec :: Spec
spec = do
  describe "validFindConflictConfigs" $
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkFindConflictConfig <$> fcs)
      `shouldBe` []
  describe "findConflicts" $ do
    context "using its default config" $
      it "generates everything required to create the task" $ do
        diaConfl <- runExceptT $ findConflicts 0 defaultFindConflictConfig
        print (snd <$> diaConfl)
        return (isRight diaConfl) `shouldReturn` True
    context "using randomly chosen configs"
      $ testFindConflictConfig cs
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
    fcs = validFindConflictConfigs cs (AdvConfig Nothing Nothing Nothing)
    pcs = validPickConflictConfigs cs
    cs  = basicAndChangeConfigs 0 6

ioPropertyWith :: Int -> (Int -> StdGen -> IO Property) -> Spec
ioPropertyWith ints f = modifyMaxSuccess (`div` 20) $
  it "generates everything required to create the task" $ property $ \x y ->
    let g  = mkStdGen x
        g' = mkStdGen y
        r  = fst $ randomR (0, ints - 1) g'
    in ioProperty $ f r g

testFindConflictConfig :: [(BasicConfig, ChangeConfig)] -> Spec
testFindConflictConfig cs = ioPropertyWith (length cs) $ \r g -> do
  ti <- runExceptT $ do
    let conf = fcs !! r
    let (r', g') = randomR (0, 1000) g
    is <- getAlloyInstances
      defaultCallAlloyConfig {
         maxInstances = Just $ toInteger r',
         timeout = Just 5000000
         }
      (petriNetFindConfl conf)
    let r'' = if r' >= length is
              then fst $ randomR (0, length is - 1) g'
              else r'
    findConflictsTaskInstance (is !! r'') $ graphLayout $ bc conf
  return $ isConflictResult f . fmap snd $ ti
  where
    bc :: FindConflictConfig -> BasicConfig
    bc = basicConfig
    fcs = validAdvConfigs >>= validFindConflictConfigs cs
    f Nothing  = False
    f (Just c) = isValidConflict c

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig cs = ioPropertyWith (length cs) $ \r g -> do
  ti <- runExceptT $ do
    let conf = cs !! r
    let (r', g') = randomR (0, 10000) g
    is <- getAlloyInstances
      defaultCallAlloyConfig {
         maxInstances = Just $ toInteger r',
         timeout = Just 5000000
         }
      (petriNetPickConfl conf)
    let r'' = if r' >= length is
              then fst $ randomR (0, length is - 1) g'
              else r'
    pickConflictsTaskInstance (is !! r'') $ graphLayout $ bc conf
  return $ isConflictResult f . fmap (fmap snd) $ ti
  where
    bc :: PickConflictConfig -> BasicConfig
    bc = basicConfig
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

isValidConflict :: Conflict -> Bool
isValidConflict c@(Conflict (t1, t2) p)
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, ('s':_) <- p = True
  | otherwise                                          = error $ show c

isConflictResult :: (a -> Bool) -> Either String a -> Property
isConflictResult p (Right c)                      = True ==> p c
isConflictResult _ (Left "no instance available") = False ==> False
isConflictResult _ (Left x)                       = error x

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

basicAndChangeConfigs :: Int -> Int -> [(BasicConfig, ChangeConfig)]
basicAndChangeConfigs low high =
  [ (BasicConfig p t ala mintoa maxtoa maxtpp minfoa maxfoa maxfpe Neato,
     ChangeConfig tcoa mtcpp fcoa mfcpe
    )
  | p      <- [mlow1 .. min 8 high]
  , t      <- [mlow1 .. min 8 high]
  , minfoa <- [t + p - 1 .. high]
  , maxfpe <- [mlow1 .. high]
  , maxfoa <- [max minfoa maxfpe .. min high (2 * p * t * maxfpe)]
  , mfcpe  <- [mlow .. min high maxfpe]
  , fcoa   <- [max mfcpe low .. minimum [high, maxfoa - minfoa, 2 * p * t * mfcpe]]
  , mintoa <- [mlow .. high]
  , maxtpp <- [mlow .. high]
  , maxtoa <- [max mintoa maxtpp .. min high (p * maxtpp)]
  , mtcpp  <- [mlow .. min high maxtpp]
  , tcoa   <- [max mtcpp low .. minimum [high, maxtoa - mintoa, mtcpp * p]]
  , ala    <- [max 2 low .. min t high]
  ]
  where
    mlow  = max 0 low
    mlow1 = max 1 low

validAdvConfigs :: [AdvConfig]
validAdvConfigs =
  AdvConfig <$> mbool <*> mbool <*> mbool
  where
    mbool = [Nothing, Just False, Just True]
