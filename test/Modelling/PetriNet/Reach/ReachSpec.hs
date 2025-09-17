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
  )

import Data.Maybe                        (isJust)
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

    it "accepts non-conflicting length hint configuration" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              maxTransitionLength = 8
              },
            rejectLongerThan = Just 7,
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
