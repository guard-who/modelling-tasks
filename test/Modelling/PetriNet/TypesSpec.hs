{-# LANGUAGE FlexibleInstances #-}
module Modelling.PetriNet.TypesSpec where

import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  Net (..),
  Node,
  PetriLike,
  SimplePetriNet,
  checkBasicConfig,
  checkChangeConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  transformNet,
  )

import qualified Data.Map                         as M (keys)

import Data.Maybe                       (isJust, fromMaybe)
import Data.Tuple.Extra                 (uncurry3)
import Test.Hspec
import Test.Hspec.QuickCheck            (prop)
import Test.QuickCheck                  (Arbitrary (..), elements, listOf)

spec :: Spec
spec = do
  describe "checkBasicConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkBasicConfig defaultBasicConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkBasicConfig defaultBasicConfig{places = 0}
          `shouldSatisfy` isJust
  describe "checkChangeConfig" $ do
    it "checks if the input for Changes is in given boundaries" $
      checkChangeConfig defaultBasicConfig defaultChangeConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with nessecary changes" $
        checkChangeConfig defaultBasicConfig defaultChangeConfig{tokenChangeOverall = -1}
          `shouldSatisfy` isJust
  describe "a Net" $ do
    context "with and without applying fromSimpleNet" $
      netProperties fromSimpleNet
    context "with and without applying toSimpleNet" $
      netProperties toSimpleNet
  where
    fromSimpleNet :: SimplePetriNet -> PetriLike Node String
    fromSimpleNet = transformNet
    toSimpleNet :: PetriLike Node String -> SimplePetriNet
    toSimpleNet = transformNet

netProperties
  :: (Arbitrary (p1 n1 String), Eq (p2 n2 String), Net p1 n1, Net p2 n2)
  => (p1 n1 String -> p2 n2 String)
  -> SpecWith ()
netProperties transform =
  context "behaves the same for" $ do
    prop "emptyNet" $ emptyNet == transform emptyNet
    prop "nodes" $ \n -> M.keys (nodes n) == M.keys (nodes $ transform n)
    prop "flow" $ \x y n -> flow x y n == flow x y (transform n)
    prop "deleteFlow" $ \x y n -> transform (deleteFlow x y n) == deleteFlow x y (transform n)
    prop "deleteNode" $ \x n -> transform (deleteNode x n) == deleteNode x (transform n)
    prop "outFlow" $ \x n -> outFlow x n == outFlow x (transform n)
    prop "mapNet" $ \n ->
      let f = fromMaybe "" . mreverseNames n
      in transform (mapNet f n) == mapNet f (transform n)
    prop "traverseNet" $ \n ->
      let f = mreverseNames n
      in fmap transform (traverseNet f n) == traverseNet f (transform n)

mreverseNames :: (Net p n, Ord b) => p n b -> b -> Maybe b
mreverseNames n x =
  let xs = M.keys (nodes n)
  in lookup x (zip xs $ reverse xs)

instance (Arbitrary a, Net p n, Ord a) => Arbitrary (p n a) where
  arbitrary = do
    ns <- listOf arbitrary
    if null ns
      then return emptyNet
      else do
      let names = fst <$> ns
      flows <- listOf $ (,,)
        <$> elements names
        <*> (abs <$> arbitrary)
        <*> elements names
      return
        . flip (foldr (uncurry3 repsertFlow)) flows
        . flip (foldr (uncurry repsertNode)) ns
        $ emptyNet
  shrink x = map (`deleteNode` x) names
    where
      names = M.keys (nodes x)
