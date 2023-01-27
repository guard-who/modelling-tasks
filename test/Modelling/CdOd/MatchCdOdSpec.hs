module Modelling.CdOd.MatchCdOdSpec where

import qualified Data.Map                         as M (lookup, null)

import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (objectConfig, timeout),
  checkMatchCdOdConfig,
  defaultMatchCdOdConfig,
  diagrams,
  getODInstances,
  matchCdOd,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.Types (
  AssociationType (..),
  ObjectConfig (..),
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Control.Monad.Except             (runExceptT)
import Data.Bifunctor                   (second)
import Data.List.Extra                  (disjoint)
import Data.Maybe                       (fromMaybe)
import Test.Hspec
import Test.QuickCheck                  (ioProperty)

spec :: Spec
spec = do
  describe "defaultMatchCdOdConfig" $
    it "is valid" $
      checkMatchCdOdConfig defaultMatchCdOdConfig `shouldBe` Nothing
  describe "matchCdOd" $
    context "using defaultMatchCdOdConfig with reduced timeouts" $
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . diagrams <$> matchCdOd cfg segment seed
        `shouldReturn` True
  describe "getODsFor" $
    it "does not generate specific false instance" $ ioProperty $ do
      let oc = ObjectConfig {
            links = (0,Just 3),
            linksPerObject = (0,Just 3),
            objects = (2,3)
            }
          config = defaultMatchCdOdConfig { objectConfig = oc }
          cd1 = (
            [("A",[]),("B",[])],
            [(Aggregation, "x", (1, Just 1), "B", "A", (1, Just 1))]
            )
          cd2 = (
            [("A", []),("B", [])],
            [(Association, "x", (1, Just 1), "A", "B", (1, Just 1))]
            )
          cd3 = ([("A", ["B"]), ("B", [])],[])
      ods <- getODInstances config cd1 cd2 cd3 2
      Right ods' <- runExceptT $ mapM alloyInstanceToOd `mapM` ods
      let get x = fromMaybe [] . M.lookup x
          flipEdge (x, y, z) = (y, x, z)
          flipOd = second (map flipEdge)
      return $ disjoint (map flipOd $ get [1] ods') (get [2] ods')
  where
    cfg = defaultMatchCdOdConfig {
      timeout = Just 5000000
      }
