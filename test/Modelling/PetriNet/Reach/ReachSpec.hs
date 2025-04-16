module Modelling.PetriNet.Reach.ReachSpec where

import qualified Data.Set                         as S

import Modelling.PetriNet.Reach.Reach (
  ReachConfig (..),
  ReachInstance (..),
  defaultReachConfig,
  generateReach,
  )
import Modelling.PetriNet.Reach.Property (
  satisfiesAtAnyState,
  )
import Modelling.PetriNet.Reach.Type (
  Net (transitions),
  State,
  Transition (..),
  )

import Data.Set                         (Set)
import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec =
  describe "generateReach" $
    it "abides minTransitionLength" $
      property $ \seed ->
        let config = defaultReachConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
            inst = generateReach config seed
            net = petriNet inst
            s = goal inst
            ts = transitions net
        in net `shouldSatisfy` hasMinTransitionLength (s ==) ts minL

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
