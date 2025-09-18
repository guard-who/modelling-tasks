module Modelling.PetriNet.Reach.ReachSpec where

import qualified Data.Set                         as S

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Reach (
  ReachConfig (..),
  NetGoalConfig (..),
  ReachInstance (..),
  NetGoal (..),
  defaultReachConfig,
  generateReach,
  checkReachConfig,
  )
import Modelling.PetriNet.Reach.Property (
  satisfiesAtAnyState,
  )
import Modelling.PetriNet.Reach.Type (
  Net (transitions),
  State,
  Transition (..),
  Capacity(..),
  Place(..),
  )

import Data.Maybe                        (isJust)
import qualified Data.Map                 as M
import Data.Set                         (Set)
import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec = do
  describe "generateReach" $
    it "abides minTransitionLength" $
      property $ \seed -> do
        let config = defaultReachConfig {
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxTransitionLength = 6,
                minTransitionLength = 6
                }
              }
            minL = minTransitionLength (netGoalConfig config)
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            s = goal (netGoal inst)
            ts = transitions net
        net `shouldSatisfy` hasMinTransitionLength (s ==) ts minL

  describe "checkReachConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultReachConfig
      checkReachConfig config `shouldBe` Nothing

    it "rejects conflicting length hint configuration" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              maxTransitionLength = 8
              },
            rejectLongerThan = Just 8,
            showLengthHint = True
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts non-conflicting length hint configuration with rejectLongerThan > maxTransitionLength" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              maxTransitionLength = 8,
              minTransitionLength = 6
              },
            rejectLongerThan = Just 10,
            showLengthHint = True
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects the problematic task2024_60-style config" $ do
      let problematicConfig = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              maxTransitionLength = 8
              },
            rejectLongerThan = Just 8,
            showLengthHint = True
            }
      checkReachConfig problematicConfig `shouldSatisfy` isJust

    it "rejects minTransitionLength > maxTransitionLength" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              minTransitionLength = 10,
              maxTransitionLength = 5
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects preconditionsRange where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              preconditionsRange = (5, Just 2)
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects postconditionsRange where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              postconditionsRange = (5, Just 2)
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects empty drawCommands" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              drawCommands = []
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects rejectLongerThan < minTransitionLength" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              minTransitionLength = 10
              },
            rejectLongerThan = Just 5,
            showLengthHint = False
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts rejectLongerThan = minTransitionLength" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              minTransitionLength = 10,
              maxTransitionLength = 10
              },
            rejectLongerThan = Just 10,
            showLengthHint = False
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects rejectLongerThan < maxTransitionLength" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              maxTransitionLength = 10,
              minTransitionLength = 5
              },
            rejectLongerThan = Just 8,
            showLengthHint = False
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts Unbounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = Unbounded
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects AllBounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = AllBounded 5
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects Bounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = Bounded (M.fromList [(Place 1, 3), (Place 2, 5)])
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration with both showTargetNet = False and showPlaceNamesInNet = False" $ do
      let config = defaultReachConfig {
            showTargetNet = False,
            showPlaceNamesInNet = False
            }
      checkReachConfig config `shouldSatisfy` isJust

hasMinTransitionLength
  :: (Ord s, Show s)
  => (State s -> Bool)
  -> Set Transition
  -> Int
  -> Net s Transition
  -> Bool
hasMinTransitionLength p ts minL n =
  not (any (satisfiesAtAnyState p n) variants)
  where
    variants = transitionVariants $ minL - 1
    transitionVariants x
      | x < 1     = [[]]
      | otherwise = [ a : as |
          a <- S.toList ts,
          as <- transitionVariants (x-1)
          ]
