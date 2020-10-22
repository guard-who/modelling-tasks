{-# LANGUAGE NamedFieldPuns #-}
module Modelling.PetriNet.ConflictsSpec (
  spec
  ) where

import Modelling.PetriNet.Conflicts (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflicts,
  pickConflicts,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig), BasicConfig (..), ChangeConfig (ChangeConfig),
  Conflict,
  FindConflictConfig (FindConflictConfig),
  PetriConflict (Conflict),
  PickConflictConfig (PickConflictConfig),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Control.Monad.Trans.Except       (runExceptT)
import Data.GraphViz                    (GraphvizCommand (Neato))
import System.Random                    (mkStdGen, randomR)
import Test.Hspec
import Test.Hspec.QuickCheck            (modifyMaxSuccess)
import Test.QuickCheck                  (Property, (==>), ioProperty, property)

spec :: Spec
spec = do
  describe "findConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConfl <- runExceptT $ findConflicts 0 defaultFindConflictConfig
        print (snd diaConfl) `shouldReturn` ()
  describe "pickConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConfls <- runExceptT $ pickConflicts 0 defaultPickConflictConfig
        print (map snd diaConfls) `shouldReturn` ()
  describe "validPickConflictConfigs" $ do
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkPickConflictConfig <$> pcs)
      `shouldBe` []
    context "pickConflicts on randomly chosen configs" $ testPickConflictConfig pcs
  describe "validFindConflictConfigs" $ do
    it "contains only valid configs" $
      take 1 (filter (/= Nothing) $ checkFindConflictConfig <$> fcs)
      `shouldBe` []
    context "findConflicts on randomly chosen configs" testFindConflictConfig
  where
    fcs = validFindConflictConfigs 0 6 (AdvConfig Nothing Nothing Nothing)
    pcs = validPickConflictConfigs 0 6

ioPropertyWith :: Int -> (Int -> IO Property) -> Spec
ioPropertyWith ints f = modifyMaxSuccess (`div` 20) $
  it "generates everything required to create the task" $ property $ \r ->
    let g = mkStdGen r
        (ri, _) = randomR (0, ints - 1) g
    in ioProperty $ f ri

testFindConflictConfig :: Spec
testFindConflictConfig = ioPropertyWith (length cs) $ \ri ->
  isConflictResult f . fmap snd <$> runExceptT (findConflicts 0 $ cs !! ri)
  where
    cs = validAdvConfigs >>= validFindConflictConfigs 0 6
    f Nothing  = False
    f (Just c) = isValidConflict c

testPickConflictConfig :: [PickConflictConfig] -> Spec
testPickConflictConfig cs = ioPropertyWith (length cs) $ \ri ->
  isConflictResult f . fmap (fmap snd) <$> runExceptT (pickConflicts 0 $ cs !! ri)
  where
    f [Just x, Nothing] = isValidConflict x
    f _                 = False

isValidConflict :: Conflict -> Bool
isValidConflict c@(Conflict (t1, t2) p)
  | ('t':x) <- t1, ('t':y) <- t2, x /= y, ('s':_) <- p = True
  | otherwise                                          = error $ show c

isConflictResult :: (a -> Bool) -> Either String a -> Property
isConflictResult p (Right c)                      = True ==> p c
isConflictResult p (Left "no instance available") = False ==> False
isConflictResult p (Left x)                       = error x

validFindConflictConfigs :: Int -> Int -> AdvConfig -> [FindConflictConfig]
validFindConflictConfigs low high aconfig =
  uncurry (`FindConflictConfig` aconfig)
  <$> basicAndChangeConfigs low high

validPickConflictConfigs :: Int -> Int -> [PickConflictConfig]
validPickConflictConfigs low high =
  uncurry PickConflictConfig <$> basicAndChangeConfigs low high

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
