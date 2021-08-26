{- |
This module provides common functions for testing Petri net modules.
-}
module Modelling.PetriNet.TestCommon (
  alloyTestConfig,
  checkConfigs,
  defaultConfigTaskGeneration,
  firstInstanceConfig,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validConfigsForPick,
  ) where

import qualified Language.Alloy.Call              as A (CallAlloyConfig (..))

import Modelling.PetriNet.Alloy         (getAlloyInstances)
import Modelling.PetriNet.Types (
  AlloyConfig (..),
  AdvConfig (AdvConfig), BasicConfig (..), ChangeConfig (ChangeConfig),
  defaultAlloyConfig,
  )

import Control.Monad.Random             (RandT, evalRandT, getRandomR)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.GraphViz                    (GraphvizCommand (Neato))
import GHC.Base                         (maxInt, minInt)
import Language.Alloy.Call (
  AlloyInstance, defaultCallAlloyConfig,
  )
import System.Random                    (StdGen, mkStdGen, randomR)

import Test.Hspec (
  Spec, context, it, shouldBe, shouldReturn,
  )
import Test.Hspec.QuickCheck            (modifyMaxSuccess)
import Test.QuickCheck (
  Arbitrary (..), Property, (==>), ioProperty, property
  )
import Test.QuickCheck.Gen              (choose)

newtype RandomGen = RandomGen { getGen :: StdGen }
  deriving Show

instance Arbitrary RandomGen where
  arbitrary = RandomGen . mkStdGen <$> choose (minInt, maxInt)

firstInstanceConfig :: AlloyConfig
firstInstanceConfig = defaultAlloyConfig {
  maxInstances = Just 1
  }

alloyTestConfig :: AlloyConfig
alloyTestConfig = defaultAlloyConfig {
  maxInstances = Nothing,
  timeout = Just 300000
  }

maxJavaInt :: Int
maxJavaInt = 2 ^ (31 :: Int) - 1

ioPropertyWith :: Int -> (Int -> StdGen -> IO Property) -> Spec
ioPropertyWith ints f = modifyMaxSuccess (`div` 20) $
  it "generates everything required to create the task" $ property $ \g g' ->
    let r  = fst $ randomR (0, ints - 1) $ getGen g'
    in ioProperty $ f r $ getGen g

testTaskGeneration
  :: (config -> String)
  -> (AlloyInstance -> RandT StdGen (ExceptT String IO) inst)
  -> (inst -> Bool)
  -> [config]
  -> Spec
testTaskGeneration alloyGen taskInst checkInst cs =
  context "using randomly chosen configs"
  $ ioPropertyWith (length cs)
  $ \r g -> do
    ti <- runExceptT $ flip evalRandT g $ do
      let conf = cs !! r
      r' <- getRandomR (0, maxJavaInt)
      is <- lift $ getAlloyInstances
        defaultCallAlloyConfig {
           A.maxInstances = Just $ toInteger r',
           A.timeout = Just 5000000
           }
        $ alloyGen conf
      r'' <- if r' >= length is
        then getRandomR (0, length is - 1)
        else return r'
      taskInst (is !! r'')
    return $ isResult checkInst ti

defaultConfigTaskGeneration
  :: (Show e, Eq e)
  => RandT StdGen (ExceptT e IO) a
  -> Int
  -> (a -> Bool)
  -> Spec
defaultConfigTaskGeneration generateInst seed checkInst =
  context "using its default config" $
    it "generates everything required to create the task" $ do
      result <- runExceptT $ evalRandT generateInst $ mkStdGen seed
      return (checkInst <$> result) `shouldReturn` Right True

checkConfigs :: (Eq b, Show b) => (a -> Maybe b) -> [a] -> Spec
checkConfigs check cs =
  it "contains only valid configs" $
    take 1 (filter (/= Nothing) $ check <$> cs)
    `shouldBe` []

isResult :: (a -> Bool) -> Either String a -> Property
isResult p (Right c)                      = True ==> p c
isResult _ (Left "no instance available") = False ==> False
isResult _ (Left x)                       = error x

validConfigsForPick :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForPick = validBasicAndChangeConfigs 0

validConfigsForFind :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForFind = validBasicAndChangeConfigs 2

validBasicAndChangeConfigs :: Int -> Int -> Int -> [(BasicConfig, ChangeConfig)]
validBasicAndChangeConfigs minala low high =
  [ (BasicConfig p t ala mintoa maxtoa maxtpp minfoa maxfoa maxfpe iso Neato True False False,
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
  , iso    <- [Nothing, Just True, Just False]
  ]
  where
    mlow  = max 0 low
    mlow1 = max 1 low

validAdvConfigs :: [AdvConfig]
validAdvConfigs =
  AdvConfig <$> mbool <*> mbool <*> mbool
  where
    mbool = [Nothing, Just False, Just True]
