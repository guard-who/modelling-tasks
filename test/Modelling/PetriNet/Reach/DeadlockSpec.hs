module Modelling.PetriNet.Reach.DeadlockSpec where

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  generateDeadlock,
  checkDeadlockConfig,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type    (Net (transitions))

import Data.Maybe                       (isJust)
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec = do
  describe "generateDeadlock" $
    it "abides minTransitionLength" $
      property $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            ts = transitions net
        net `shouldSatisfy`
          hasMinTransitionLength (null . successors net) ts minL

  describe "checkDeadlockConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultDeadlockConfig
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects conflicting length hint configuration" $ do
      let config = defaultDeadlockConfig {
            maxTransitionLength = 8,
            rejectLongerThan = Just 8,
            showLengthHint = True
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts non-conflicting length hint configuration" $ do
      let config = defaultDeadlockConfig {
            maxTransitionLength = 8,
            rejectLongerThan = Just 7,
            showLengthHint = True
            }
      checkDeadlockConfig config `shouldBe` Nothing
