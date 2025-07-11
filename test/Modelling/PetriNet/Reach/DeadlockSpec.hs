module Modelling.PetriNet.Reach.DeadlockSpec where

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
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
