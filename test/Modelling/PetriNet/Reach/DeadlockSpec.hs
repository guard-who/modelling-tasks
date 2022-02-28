module Modelling.PetriNet.Reach.DeadlockSpec where

import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  generateDeadlock,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type    (Net (transitions))

import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec =
  describe "generateDeadlock" $
    it "abides minTransitionLength" $
      property $ \seed ->
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
            n = petriNet $ generateDeadlock config seed
            ts = transitions n
        in n `shouldSatisfy`
           hasMinTransitionLength (null . successors n) ts minL
