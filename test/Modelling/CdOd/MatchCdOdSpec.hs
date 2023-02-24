module Modelling.CdOd.MatchCdOdSpec where

import qualified Data.ByteString.Char8            as BS (pack)
import qualified Data.Map                         as M (lookup, null)

import Modelling.Common                 (withUnitTests)
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (maxInstances, objectConfig),
  applyChanges,
  checkMatchCdOdConfig,
  defaultMatchCdOdConfig,
  diagrams,
  getODInstances,
  matchCdOd,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectConfig (..),
  Od,
  Relationship (..),
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad                    ((>=>))
import Control.Monad.Random             (randomIO)
import Control.Monad.Except             (runExceptT)
import Data.Maybe                       (fromMaybe)
import Language.Alloy.Debug             (parseInstance)
import Test.Hspec
import Test.QuickCheck                  (ioProperty)

spec :: Spec
spec = do
  describe "defaultMatchCdOdConfig" $
    it "is valid" $
      checkMatchCdOdConfig defaultMatchCdOdConfig `shouldBe` Nothing
  describe "matchCdOd" $
    context "using defaultMatchCdOdConfig with limited instances" $
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
  withUnitTests "applyChanges" does dir "hs" $ shouldReturn . getResult
  where
    does = "generates expected class diagrams"
    dir = "test/unit/Modelling/CdOd/MatchCdOd"
    getResult = fmap (either (error . show) id)
      . runExceptT
      . parseInstance
      . BS.pack
      >=> fmap show . applyChanges
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
      maxInstances = Just 20
      }

getOdsFor :: Cd -> Cd -> IO ([Od], [Od])
getOdsFor cd1 cd2 = do
  let cd3 = ClassDiagram {
        classNames = ["A", "B"],
        relationships = [Inheritance {subClass = "A", superClass = "B"}]
        }
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

cdAInheritsBandAtoB :: Cd
cdAInheritsBandAtoB = ClassDiagram {
  classNames = ["A", "B"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    associationX "A" "B"
    ]
  }

associationX :: c -> c -> Relationship c String
associationX from to = Association {
  associationName = "x",
  associationFrom = LimitedLinking {
    linking = from,
    limits = (1, Just 1)
    },
  associationTo = LimitedLinking {
    linking = to,
    limits = (1, Just 1)
    }
  }

cdAggregateBofAs :: Cd
cdAggregateBofAs = ClassDiagram {
  classNames = ["A", "B"],
  relationships = [
    Aggregation {
      aggregationName = "x",
      aggregationPart = LimitedLinking {
        linking = "A",
        limits = (1, Just 1)
        },
      aggregationWhole = LimitedLinking {
        linking = "B",
        limits = (1, Just 1)
        }
      }
    ]
  }

cdComposeBofAs :: Cd
cdComposeBofAs = ClassDiagram {
  classNames = ["A", "B"],
  relationships = [
    Composition {
      compositionName = "x",
      compositionPart = LimitedLinking {
        linking = "A",
        limits = (1, Just 1)
        },
      compositionWhole = LimitedLinking {
        linking = "B",
        limits = (1, Just 1)
        }
      }
    ]
  }

cdAtoB :: Cd
cdAtoB = ClassDiagram {
  classNames = ["A", "B"],
  relationships = [associationX "A" "B"]
  }

cdBtoA :: Cd
cdBtoA = ClassDiagram {
  classNames = ["A", "B"],
  relationships = [associationX "B" "A"]
  }
