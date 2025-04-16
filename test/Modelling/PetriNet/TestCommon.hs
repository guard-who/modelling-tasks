{-# LANGUAGE ScopedTypeVariables #-}
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
  validGraphConfig,
  ) where

import Capabilities.Alloy               (getInstances)
import Capabilities.Alloy.IO            ()
import Modelling.PetriNet.Alloy         (TaskGenerationException (..))
import Modelling.PetriNet.Types (
  AlloyConfig (..),
  AdvConfig (AdvConfig), BasicConfig (..), ChangeConfig (ChangeConfig),
  GraphConfig (..),
  defaultAlloyConfig,
  )

import Control.Monad.Catch              (MonadThrow (throwM), MonadCatch (catch))
import Control.Monad.Random             (RandT, evalRandT, getRandomR)
import Data.GraphViz                    (GraphvizCommand (Neato))
import GHC.Base                         (maxInt, minInt)
import Language.Alloy.Call (
  AlloyInstance,
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
  maxInstances = Just 1,
  timeout = Nothing
  }

alloyTestConfig :: AlloyConfig
alloyTestConfig = defaultAlloyConfig {
  maxInstances = Just 1000,
  timeout = Nothing
  }

maxJavaInt :: Int
maxJavaInt = 2 ^ (31 :: Int) - 1

ioPropertyWith :: Int -> (Int -> StdGen -> IO Property) -> Spec
ioPropertyWith range f = modifyMaxSuccess (`div` 20) $
  it "generates everything required to create the task" $ property $ \g g' ->
    let r = fst $ randomR (0, range - 1) $ getGen g'
    in ioProperty $ f r $ getGen g

testTaskGeneration
  :: (config -> String)
  -> (AlloyInstance -> RandT StdGen IO inst)
  -> (inst -> Bool)
  -> [config]
  -> Spec
testTaskGeneration alloyGen taskInst checkInst cs =
  context "using randomly chosen configs"
  $ ioPropertyWith (length cs)
  $ \r g -> isResult checkInst
    $ flip evalRandT g $ do
      let conf = cs !! r
      r' <- getRandomR (1, maxJavaInt)
      is <- getInstances
        (Just $ toInteger r')
        (Just 5000000)
        $ alloyGen conf
      if null is
        then throwM NoInstanceAvailable
        else do
        let instances = length is
        r'' <- if r' >= instances
          then getRandomR (0, instances - 1)
          else return r'
        taskInst (is !! r'')

defaultConfigTaskGeneration
  :: RandT StdGen IO a
  -> Int
  -> (a -> Bool)
  -> Spec
defaultConfigTaskGeneration generateInst seed checkInst =
  context "using its default config" $
    it "generates everything required to create the task" $
      checkInst <$> evalRandT generateInst (mkStdGen seed)
      `shouldReturn` True

checkConfigs :: (Eq b, Show b) => (a -> Maybe b) -> [a] -> Spec
checkConfigs check cs =
  it "contains only valid configs" $
    take 1 (filter (/= Nothing) $ map check cs)
    `shouldBe` []

isResult :: MonadCatch m => (a -> Bool) -> m a -> m Property
isResult p m = catch
  ((True ==>) . p <$> m)
  (\(_ :: TaskGenerationException) -> pure $ False ==> False)

validConfigsForPick :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForPick = validBasicAndChangeConfigs 0

validConfigsForFind :: Int -> Int -> [(BasicConfig, ChangeConfig)]
validConfigsForFind = validBasicAndChangeConfigs 2

validGraphConfig :: GraphConfig
validGraphConfig = GraphConfig {
  graphLayouts = [Neato],
  hidePlaceNames = False,
  hideTransitionNames = False,
  hideWeight1 = True
  }

validBasicAndChangeConfigs :: Int -> Int -> Int -> [(BasicConfig, ChangeConfig)]
validBasicAndChangeConfigs minActive low high =
  [ (BasicConfig {
       places = p,
       transitions = t,
       atLeastActive = active,
       flowOverall = (minFlow, maxFlow),
       isConnected = connected,
       maxTokensPerPlace = maxPerPlace,
       maxFlowPerEdge = maxPerEdge,
       tokensOverall = (minToken, maxToken)
       },
     ChangeConfig tokenChange maxPlaceChange flowChange maxEdgeChange
    )
  | p <- [low1 .. min 8 high]
  , t <- [low1 .. min 8 high]
  , minFlow <- [t + p - 1 .. high]
  , maxPerEdge <- [low1 .. high]
  , maxFlow <- [max minFlow maxPerEdge .. min high (2 * p * t * maxPerEdge)]
  , maxEdgeChange <- [low0 .. min high maxPerEdge]
  , let maxFlowChange = minimum [high, maxFlow - minFlow, 2 * p * t * maxEdgeChange]
  , flowChange <- [max maxEdgeChange low .. maxFlowChange]
  , minToken <- [low0 .. high]
  , maxPerPlace <- [low0 .. high]
  , maxToken <- [max minToken maxPerPlace .. min high (p * maxPerPlace)]
  , maxPlaceChange <- [low0 .. min high maxPerPlace]
  , let maxTokenChange = minimum [high, maxToken - minToken, maxPlaceChange * p]
  , tokenChange <- [max maxPlaceChange low .. maxTokenChange]
  , active <- [max minActive low .. min t high]
  , connected <- maybeBool
  ]
  where
    low0 = max 0 low
    low1 = max 1 low

validAdvConfigs :: [AdvConfig]
validAdvConfigs = [ AdvConfig x y z |
    x <- maybeBool,
    y <- maybeBool,
    z <- maybeBool
    ]

maybeBool :: [Maybe Bool]
maybeBool = [Nothing, Just False, Just True]
