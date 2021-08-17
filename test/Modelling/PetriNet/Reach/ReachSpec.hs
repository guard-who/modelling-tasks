module Modelling.PetriNet.Reach.ReachSpec where

import qualified Data.Set                         as S

import Modelling.PetriNet.Reach.Reach (
  Config (..),
  defaultReachConfig,
  generateReach,
  )
import Modelling.PetriNet.Reach.Property (
  satisfiesAtAnyState,
  )
import Modelling.PetriNet.Reach.Type    (Transition (..), Net (transitions), State)

import Data.Set                         (Set)
import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec =
  describe "generateReach" $ do
    it "abides minTransitionLength" $
      property $ \seed ->
        let config = defaultReachConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
            (net, s) = generateReach config seed
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
  not (any (satisfiesAtAnyState p n) tvs)
  where
    tvs = transitionVariants $ minL - 1
    transitionVariants x
      | x < 1     = [[]]
      | otherwise = (:) <$> S.toList ts <*> transitionVariants (x - 1)
