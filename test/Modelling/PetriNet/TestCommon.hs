{- |
This module provides common functions for testing Petri net modules.
-}
module Modelling.PetriNet.TestCommon (
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  ) where

import Modelling.PetriNet.Alloy         (getAlloyInstances)
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig), BasicConfig (..), ChangeConfig (ChangeConfig),
  )

import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.GraphViz                    (GraphvizCommand (Neato))
import GHC.Base                         (maxInt, minInt)
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig (..), defaultCallAlloyConfig,
  )
import System.Random                    (StdGen, mkStdGen, randomR)
import Test.Hspec                       (Spec, it)
import Test.Hspec.QuickCheck            (modifyMaxSuccess)
import Test.QuickCheck (
  Arbitrary (..), Property, ioProperty, property
  )
import Test.QuickCheck.Gen              (choose)

newtype RandomGen = RandomGen { getGen :: StdGen }
  deriving Show

instance Arbitrary RandomGen where
  arbitrary = RandomGen . mkStdGen <$> choose (minInt, maxInt)

maxJavaInt :: Int
maxJavaInt = 2 ^ (31 :: Int) - 1
 
ioPropertyWith :: Int -> (Int -> StdGen -> IO Property) -> Spec
ioPropertyWith ints f = modifyMaxSuccess (`div` 20) $
  it "generates everything required to create the task" $ property $ \g g' ->
    let r  = fst $ randomR (0, ints - 1) $ getGen g'
    in ioProperty $ f r $ getGen g

testTaskGeneration
  :: (config -> String)
  -> (AlloyInstance -> config -> ExceptT String IO inst)
  -> (Either String inst -> Property)
  -> [config]
  -> Spec
testTaskGeneration alloyGen taskInst checkInst cs =
  ioPropertyWith (length cs) $ \r g -> do
    ti <- runExceptT $ do
      let conf = cs !! r
      let (r', g') = randomR (0, maxJavaInt) g
      is <- getAlloyInstances
        defaultCallAlloyConfig {
           maxInstances = Just $ toInteger r',
           timeout = Just 5000000
           }
        $ alloyGen conf
      let r'' = if r' >= length is
                then fst $ randomR (0, length is - 1) g'
                else r'
      taskInst (is !! r'') conf
    return $ checkInst ti

validConfigsForPick :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForPick = validBasicAndChangeConfigs 0

validConfigsForFind :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForFind = validBasicAndChangeConfigs 2

validBasicAndChangeConfigs :: Int -> Int -> Int -> [(BasicConfig, ChangeConfig)]
validBasicAndChangeConfigs minala low high =
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
  , ala    <- [max minala low .. min t high]
  ]
  where
    mlow  = max 0 low
    mlow1 = max 1 low

validAdvConfigs :: [AdvConfig]
validAdvConfigs =
  AdvConfig <$> mbool <*> mbool <*> mbool
  where
    mbool = [Nothing, Just False, Just True]
