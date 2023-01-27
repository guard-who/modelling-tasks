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
  Od,
  Syntax,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Control.Monad.Except             (runExceptT)
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
  describe "getODsFor" $ do
    it "does not generate specific false instance" $ ioProperty $ do
      getOdsFor cdAggregateBofAs cdAtoB
      `shouldReturn` ([], [])
    it "generates correct ODs for association and opposing association" $
      getOdsFor cdAtoB cdBtoA
      `shouldReturn` opposingOd
    it "generates correct ODs for aggregation and opposing association" $
      getOdsFor cdAggregateBofAs cdBtoA
      `shouldReturn` opposingOd
    it "generates correct ODs for composition and opposing association" $
      getOdsFor cdComposeBofAs cdBtoA
      `shouldReturn` opposingOd
    it "generates correct ODs for inheritance and association" $
      getOdsFor cdAInheritsBandAtoB cdAtoB
      `shouldReturn` inheritOd
    it "generates correct ODs for inheritance and aggregation" $
      getOdsFor cdAInheritsBandAtoB cdAggregateBofAs
      `shouldReturn` inheritOd
    it "generates correct ODs for inheritance and composition" $
      getOdsFor cdAInheritsBandAtoB cdComposeBofAs
      `shouldReturn` inheritOd
  where
    opposingOd = (
      [(["A$0", "B$0"], [(1, 0, "x")])],
      [(["A$0", "B$0"], [(0, 1, "x")])]
      )
    inheritOd = (
      [(["A$0", "A$1"], [(0, 0, "x"), (1, 1, "x")]),
       (["A$0", "A$1"], [(0, 1, "x"), (1, 0, "x")])],
      [(["A$0", "B$0"], [(1, 0, "x")])]
      )
    cfg = defaultMatchCdOdConfig {
      timeout = Just 5000000
      }

getOdsFor :: Syntax -> Syntax -> IO ([Od], [Od])
getOdsFor cd1 cd2 = do
  let cd3 = ([("A", ["B"]), ("B", [])],[])
  ods <- getODInstances fewObjects cd1 cd2 cd3 2
  Right ods' <- runExceptT $ mapM alloyInstanceToOd `mapM` ods
  return (get [1] ods', get [2] ods')
  where
    get x = fromMaybe [] . M.lookup x
    fewObjects = defaultMatchCdOdConfig { objectConfig = oc }
    oc = ObjectConfig {
      links = (0, Just 2),
      linksPerObject = (0, Just 2),
      objects = (2, 2)
      }

cdAInheritsBandAtoB :: Syntax
cdAInheritsBandAtoB = (
  [("A", ["B"]), ("B", [])],
  [(Association, "x", (1, Just 1), "A", "B", (1, Just 1))]
  )

cdAggregateBofAs :: Syntax
cdAggregateBofAs = (
  [("A", []), ("B", [])],
  [(Aggregation, "x", (1, Just 1), "B", "A", (1, Just 1))]
  )

cdComposeBofAs :: Syntax
cdComposeBofAs = (
  [("A", []), ("B", [])],
  [(Composition, "x", (1, Just 1), "B", "A", (1, Just 1))]
  )

cdAtoB :: Syntax
cdAtoB = (
  [("A", []),("B", [])],
  [(Association, "x", (1, Just 1), "A", "B", (1, Just 1))]
  )

cdBtoA :: Syntax
cdBtoA = (
  [("A", []),("B", [])],
  [(Association, "x", (1, Just 1), "B", "A", (1, Just 1))]
  )
