{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import qualified Language.Alloy.Call              as Alloy (getInstances)

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.CD2Alloy.Transform (
  LinguisticReuse (None),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.Output            (drawCd, drawOdFromInstance)
import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectConfig (objectLimits),
  ObjectProperties (..),
  Relationship (..),
  defaultCdDrawSettings,
  fromClassDiagram,
  maxFiveObjects,
  relationshipName,
  )

import Control.Monad.Random             (evalRandT, getStdGen)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Foldable                    (toList)
import Data.GraphViz                    (DirType (..))
import Data.Maybe                       (mapMaybe)
import Data.Ratio                       ((%))

v :: Relationship String String
v = Aggregation {
  aggregationName = "v",
  aggregationPart = LimitedLinking {
    linking = "B",
    limits = (1, Just 1)
    },
  aggregationWhole = LimitedLinking {
    linking = "C",
    limits = (0, Nothing)
    }
  }

w :: Relationship String String
w = Association {
  associationName = "w",
  associationFrom = LimitedLinking {
    linking = "A",
    limits = (-1, Just 2)
    },
  associationTo = LimitedLinking {
    linking = "C",
    limits = (-1, Just 2)
    }
  }

x :: Relationship String String
x = Composition {
  compositionName = "x",
  compositionPart = LimitedLinking {
    linking = "B",
    limits = (1, Just 1)
    },
  compositionWhole = LimitedLinking {
    linking = "C",
    limits = (0, Just 1)
    }
  }

y' :: Relationship String String
y' = Association {
  associationName = "y",
  associationFrom = LimitedLinking {
    linking = "D",
    limits = (0, Just 1)
    },
  associationTo = LimitedLinking {
    linking = "A",
    limits = (0, Nothing)
    }
  }

y'' :: Relationship String String
y'' = Association {
  associationName = "y",
  associationFrom = LimitedLinking {
    linking = "D",
    limits = (0, Nothing)
    },
  associationTo = LimitedLinking {
    linking = "A",
    limits = (0, Nothing)
    }
  }

z' :: Relationship String String
z' = Aggregation {
  aggregationName = "z",
  aggregationPart = LimitedLinking {
    linking = "C",
    limits = (2, Nothing)
    },
  aggregationWhole = LimitedLinking {
    linking = "D",
    limits = (0, Nothing)
    }
  }

z'' :: Relationship String String
z'' = Aggregation {
  aggregationName = "z",
  aggregationPart = LimitedLinking {
    linking = "C",
    limits = (2, Nothing)
    },
  aggregationWhole = LimitedLinking {
    linking = "D",
    limits = (0, Just 1)
    }
  }

inh1 :: Relationship String String
inh1 = Inheritance {
  subClass = "A",
  superClass = "C"
  }

inh2 :: Relationship String String
inh2 = Inheritance {
  subClass = "B",
  superClass = "A"
  }

inh3 :: Relationship String String
inh3 = Inheritance {
  subClass = "C",
  superClass = "A"
  }

a :: Relationship String String
a = Association {
  associationName = "a",
  associationFrom = LimitedLinking {
    linking = "A",
    limits = (1, Just 1)
    },
  associationTo = LimitedLinking {
    linking = "B",
    limits = (1, Just 1)
    }
  }

b :: Relationship String String
b = Association {
  associationName = "b",
  associationFrom = LimitedLinking {
    linking = "C",
    limits = (1, Just 1)
    },
  associationTo = LimitedLinking {
    linking = "B",
    limits = (1, Just 1)
    }
  }

main :: IO ()
main = do
  -- names are required
  let cd0 = ClassDiagram ["A", "B", "C"] [a, b, inh1]
      cd1 = ClassDiagram ["A", "B", "C"] [a, inh1]
      cd2 = ClassDiagram ["A", "B", "C"] [b, inh1]
  drawCdAndOdsFor Nothing "names" [cd0, cd1, cd2] "cd1 and cd2"
  drawCdAndOdsFor Nothing "names" [cd0, cd1, cd2] "cd1 and not cd2"
  drawCdAndOdsFor Nothing "names" [cd0, cd1, cd2] "cd2 and not cd1"
  drawCdAndOdsFor Nothing "names" [cd0, cd1, cd2] "cd0 and not cd1 and not cd2"
  -- no matter where limits
  drawCdsAndOds "y-limits-" y' z'
  drawCdsAndOds "z-limits-" y'' z''

drawCdsAndOds
  :: String
  -> Relationship String String
  -> Relationship String String
  -> IO ()
drawCdsAndOds c y z =
  let cds = [ClassDiagram classes [x, y, z, inh1, inh2],
             ClassDiagram classes [v, x, y, z, inh1, inh2],
             ClassDiagram classes [x, y, z, inh2],
             ClassDiagram classes [x, y, z, inh3, inh2],
             ClassDiagram classes [w, x, y, z, inh2]]
  in foldr
     ((>>) . (\(i, cd) -> drawCdAndOdsFor (Just 1) (c ++ show i) [cd] "cd0"))
     (return ())
     $ zip [0..] cds
  where
    classes = map (:[]) ['A' .. 'D']

drawCdAndOdsFor
  :: Maybe Integer
  -> String
  -> [Cd]
  -> String
  -> IO ()
drawCdAndOdsFor is c cds cmd = do
  mapM_
    (\(cd, i) -> drawCd' (fromClassDiagram cd) i >>= putStrLn)
    $ zip cds [0..]
  let mergedParts = foldr mergeParts (head parts) $ tail parts
      parts = getParts allRelationshipNames
      allRelationships = concatMap relationships cds
      allRelationshipNames = mapMaybe relationshipName allRelationships
  let parts' = combineParts mergedParts ++ createRunCommand
        cmd
        Nothing
        3
        maxThreeObjects
        allRelationships
  ods <- Alloy.getInstances is parts'
  g <- getStdGen
  let possibleLinks = toList allRelationshipNames
  flip evalRandT g $
    mapM_ (\(od, i) -> drawOd possibleLinks od i >>= lift . putStrLn)
    $ zip (maybe id (take . fromInteger) is ods) [1..]
  where
    drawOd allRelationshipNames od i = drawOdFromInstance
      od
      Nothing
      allRelationshipNames
      Nothing
      Back
      True
      (c ++ '-' : shorten cmd ++ "-od" ++ show i ++ ".svg")
    drawCd' cd i =
      drawCd defaultCdDrawSettings mempty cd (c ++ "-cd" ++ show i ++ ".svg")
    maxThreeObjects = maxFiveObjects { objectLimits = (1, 3) }
    getParts relationshipNames = zipWith (cdToAlloy relationshipNames) cds [0..]
    cdToAlloy relationshipNames cd i = transform
      None
      cd
      (Just relationshipNames)
      []
      maxThreeObjects
      objectProperties
      (show i)
      ""
    shorten (' ':'a':'n':'d':' ':'c':'d':ys) =
      "and" ++ shorten ys
    shorten (' ':'a':'n':'d':' ':'n':'o':'t':' ':'c':'d':ys) =
      "not" ++ shorten ys
    shorten (y:ys) = y : shorten ys
    shorten []     = []
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 0 % 1,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Nothing
      }
