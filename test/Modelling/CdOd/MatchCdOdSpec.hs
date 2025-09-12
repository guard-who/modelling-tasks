module Modelling.CdOd.MatchCdOdSpec where

import qualified Data.Map                         as M (lookup, null)

import Capabilities.Alloy.IO            ()
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (objectConfig),
  checkMatchCdOdConfig,
  defaultMatchCdOdConfig,
  defaultMatchCdOdInstance,
  diagrams,
  getODInstances,
  matchCdOd,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  Od,
  Relationship (..),
  normaliseObjectDiagram,
  relationshipName,
  )
import Modelling.Auxiliary.Common       (oneOf)

import Control.Monad.Random             (randomIO)
import Control.Monad.Except             (runExceptT)
import Data.List                        (sort)
import Data.Maybe                       (mapMaybe)
import Data.Tuple.Extra                 (both)
import Test.Hspec
import Test.QuickCheck                  (ioProperty)

spec :: Spec
spec = do
  describe "defaultMatchCdOdConfig" $
    it "is valid" $
      checkMatchCdOdConfig defaultMatchCdOdConfig `shouldBe` Nothing
  describe "matchCdOd" $
    context "using defaultMatchCdOdConfig" $ do
      it "generates an instance" $
        do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          not . M.null . diagrams
            <$> matchCdOd defaultMatchCdOdConfig segment seed
        `shouldReturn` True
      it "reproducible generates defaultMatchCdOdInstance" $
        matchCdOd defaultMatchCdOdConfig 0 0
        `shouldReturn` defaultMatchCdOdInstance
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
    opposingOd = both (:[]) (
      ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "a", objectClass = "A"},
          Object {isAnonymous = False, objectName = "b", objectClass = "B"}],
        links = [
          Link {linkLabel = "x", linkFrom = "a", linkTo = "b"}]
        },
      ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "a", objectClass = "A"},
          Object {isAnonymous = False, objectName = "b", objectClass = "B"}],
        links = [
          Link {linkLabel = "x", linkFrom = "b", linkTo = "a"}]
        }
      )
    inheritOd = (
      [
        ObjectDiagram {
          objects = [
            Object {isAnonymous = False, objectName = "a", objectClass = "A"},
            Object {isAnonymous = False, objectName = "a1", objectClass = "A"}
            ],
          links = [
            Link {linkLabel = "x", linkFrom = "a", linkTo = "a"},
            Link {linkLabel = "x", linkFrom = "a1", linkTo = "a1"}
            ]
          },
        ObjectDiagram {
          objects = [
            Object {isAnonymous = False, objectName = "a", objectClass = "A"},
            Object {isAnonymous = False, objectName = "a1", objectClass = "A"}
            ],
          links = [
            Link {linkLabel = "x", linkFrom = "a", linkTo = "a1"},
            Link {linkLabel = "x", linkFrom = "a1", linkTo = "a"}
            ]
          }
        ],
      [
        ObjectDiagram {
          objects = [
            Object {isAnonymous = False, objectName = "a", objectClass = "A"},
            Object {isAnonymous = False, objectName = "b", objectClass = "B"}
            ],
          links = [
            Link {linkLabel = "x", linkFrom = "a", linkTo = "b"}
            ]
          }
        ]
      )

getOdsFor :: Cd -> Cd -> IO ([Od], [Od])
getOdsFor cd1 cd2 = do
  let cd3 = ClassDiagram {
        classNames = ["A", "B"],
        relationships = [Inheritance {subClass = "A", superClass = "B"}]
        }
  ods <- getODInstances fewObjects cd1 cd2 cd3 2
  let possibleLinks = concatMap
        (mapMaybe relationshipName . relationships)
        [cd1, cd2, cd3]
  Right ods' <- runExceptT $ mapM (alloyInstanceToOd Nothing possibleLinks) `mapM` ods
  return (get [1] ods', get [2] ods')
  where
    get x = sort . maybe [] (map normaliseObjectDiagram) . M.lookup x
    fewObjects = defaultMatchCdOdConfig { objectConfig = oc }
    oc = ObjectConfig {
      linkLimits = (0, Just 2),
      linksPerObjectLimits = (0, Just 2),
      objectLimits = (2, 2)
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
