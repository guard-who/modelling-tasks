module Modelling.CdOd.GenerateSpec where

import Modelling.CdOd.Edges             (hasAssociationAtOneSuperclass)
import Modelling.CdOd.Generate          (generate)
import Modelling.CdOd.Types             (ClassConfig (..))

import Test.Hspec
import Test.QuickCheck                  (ioProperty)

spec :: Spec
spec =
  describe "generate" $ do
    it "generates non trivial inheritance instances" $
      ioProperty $ do
        c <- generate (Just True) classConfig 10
        return $ c `shouldSatisfy` uncurry hasAssociationAtOneSuperclass
    it "generates non trivial inheritance instances" $
      ioProperty $ do
        c <- generate (Just False) classConfig 10
        return $ c `shouldSatisfy` not . uncurry hasAssociationAtOneSuperclass


classConfig :: ClassConfig
classConfig = ClassConfig {
  classes      = (4, 4),
  aggregations = (0, Just 2),
  associations = (0, Just 2),
  compositions = (0, Just 1),
  inheritances = (1, Just 2)
  }
