{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.CdOd.CdAndChanges.Transform (
  transform,
  transformChanges,
  transformGetNextFix,
  transformImproveCd,
  transformNoChanges,
  ) where

import Modelling.CdOd.Types (
  Cd,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Relationship (..),
  RelationshipProperties (..),
  maxRels,
  towardsValidProperties,
  )

import Data.Bool                        (bool)
import Data.FileEmbed                   (embedStringFile)
import Data.Functor                     ((<&>))
import Data.List                        (intercalate, unzip4)
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (__i, i)

transformWith
  :: ClassConfig
  -> Either (ClassDiagram String relationship) RelationshipProperties
  -> (Int, [String], String)
  -> String
transformWith config cdOrProperties (cs, predicates, part) =
  removeLine $(embedStringFile "alloy/cd/assoclimits.als")
  ++ removeLines 3 $(embedStringFile "alloy/cd/generate.als")
  ++ either givenClassDiagram (classDiagram config) cdOrProperties
  ++ part
  ++ createRunCommand config predicates cs

{-|
Create Alloy code for the generation of a single class diagram with the
given properties.
-}
transformNoChanges
  :: ClassConfig
  -> RelationshipProperties
  -> Maybe Bool
  -> String
transformNoChanges config properties withNonTrivialInheritance =
  transformWith config (Right properties) (0, [], part)
  where
    part = [__i|
      fact{
      #{nonTrivialInheritanceConstraint "Inheritance" "Assoc" withNonTrivialInheritance}
      }
      |]

nonTrivialInheritanceConstraint :: String -> String -> Maybe Bool -> String
nonTrivialInheritanceConstraint inheritances assocs withNonTrivialInheritance =
  (`foldMap` trivialInh) $ \x -> [i|  #{withInheritance}
  #{x} i : #{inheritances} | i.to in (#{assocs}.from + #{assocs}.to)|]
  where
    trivialInh = withNonTrivialInheritance
      <&> bool "no" "all"
    withInheritance = maybe
      ""
      (bool "" "some Inheritance")
      withNonTrivialInheritance

transform :: ClassConfig -> RelationshipProperties -> Maybe Bool -> String
transform config props withNonTrivialInheritance =
  transformWith config (Right props)
  $ matchCdOdChanges config withNonTrivialInheritance

transformChanges
  :: ClassConfig
  -> RelationshipProperties
  -> Maybe ClassConfig
  -> [RelationshipProperties]
  -> String
transformChanges config props mconfig propss =
  transformWith config (Right props) $ changes mconfig propss

transformImproveCd
  :: ClassDiagram String relationship
  -- ^ the generated CD
  -> ClassConfig
  -- ^ the configuration used for generating the CD
  -> RelationshipProperties
  -- ^ the properties of the original CD
  -> String
transformImproveCd cd config properties = transformWith config (Left cd)
  $ changes Nothing [towardsValidProperties properties]

transformGetNextFix
  :: Maybe Cd
  -> ClassConfig
  -> RelationshipProperties
  -> String
transformGetNextFix maybeCd config properties = transformWith
  config
  (maybe (Right properties) Left maybeCd)
  (n, ps, part ++ restrictChanges)
  where
    (n, ps, part) = changes Nothing [towardsValidProperties properties]
    restrictChanges = [i|
fact restrictChanges {
  no Change.add
}
|]

nameRelationships
  :: ClassDiagram className relationshipName
  -> [(String, Relationship className relationshipName)]
nameRelationships ClassDiagram {relationships} = zip
  (map (("Relationship" ++) . show) [0 :: Int ..])
  relationships

givenClassDiagram :: ClassDiagram String relationship -> String
givenClassDiagram cd@ClassDiagram {classNames} = [i|
//////////////////////////////////////////////////
// Given CD
//////////////////////////////////////////////////

#{concatMap classSig classNames}
#{concatMap relationshipSig namedRelationships}
pred cd {
  Class = #{unionOf classNames}
#{concatMap relationshipConstraints namedRelationships}
  Assoc = Association + Aggregation + Composition
  Relationship = Assoc + Inheritance
  Association - Change.add = #{unionOf $ concat associations}
  Aggregation - Change.add = #{unionOf $ concat aggregations}
  Composition - Change.add = #{unionOf $ concat compositions}
  Inheritance - Change.add = #{unionOf $ concat inheritances}
}
|]
  where
    unionOf xs
      | null xs = "none"
      | otherwise = intercalate " + " xs
    namedRelationships = nameRelationships cd
    (associations, aggregations, compositions, inheritances) =
      unzip4 $ map assocName namedRelationships
    assocName (name, x) = case x of
      Association {} -> ([name], [], [], [])
      Aggregation {} -> ([], [name], [], [])
      Composition {} -> ([], [], [name], [])
      Inheritance {} -> ([], [], [], [name])
    classSig :: String -> String
    classSig x = [i|one sig #{x} extends Class {}\n|]
    relationshipSig :: (String, Relationship String relationship) -> String
    relationshipSig (name, x) = case x of
      Association {} -> [i|one sig #{name} extends Association {}\n|]
      Aggregation {} -> [i|one sig #{name} extends Aggregation {}\n|]
      Composition {} -> [i|one sig #{name} extends Composition {}\n|]
      Inheritance {} -> [i|one sig #{name} extends Inheritance {}\n|]
    relationshipConstraints (name, x) = case x of
      Association {..} -> limitsConstraints name associationFrom associationTo
      Aggregation {..} -> limitsConstraints name aggregationPart aggregationWhole
      Composition {..} -> limitsConstraints name compositionPart compositionWhole
      Inheritance {..} -> [i|  #{name}.from = #{subClass}\n|]
        ++ [i|  #{name}.to = #{superClass}\n|]
    limitsConstraints x from to =
      limitConstraints "from" x from ++ limitConstraints "to" x to
    limitConstraints :: String -> String -> LimitedLinking String -> String
    limitConstraints
      what
      x
      LimitedLinking {linking = destination, limits = (low, high)} =
        [i|  #{x}.#{what} = #{destination}\n|]
        ++ [i|  #{x}.#{what}Lower = #{limit low}\n|]
        ++ [i|  #{x}.#{what}Upper = #{limit $ fromMaybe (-1) high}\n|]
    limit 0 = "Zero"
    limit 1 = "One"
    limit 2 = "Two"
    limit _ = "Star"

classDiagram :: ClassConfig -> RelationshipProperties -> String
classDiagram config props = [i|
//////////////////////////////////////////////////
// Basic CD
//////////////////////////////////////////////////

pred cd {
  let Assoc2 = Assoc - Change.add,
      Association2 = Association - Change.add,
      Aggregation2 = Aggregation - Change.add,
      Composition2 = Composition - Change.add,
      Relationship2 = Relationship - Change.add,
      Inheritance2 = Inheritance - Change.add {
    classDiagram [Assoc2, Composition2, Inheritance2, Relationship2,
      #{wrongAssocs props}, #{wrongCompositions props}, #{selfRelationships props},
      #{selfInheritances props},
      #{maybeToAlloySet $ hasDoubleRelationships props},
      #{maybeToAlloySet $ hasReverseRelationships props},
      #{hasReverseInheritances props},
      #{maybeToAlloySet $ hasMultipleInheritances props},
      #{hasNonTrivialInheritanceCycles props},
      #{hasCompositionCycles props},
      #{maybeToAlloySet $ hasCompositionsPreventingParts props},
      #{maybeToAlloySet $ hasThickEdges props}]
    #{fst $ associationLimits config} <= \#Association2
    \#Association2 <= #{upper $ associationLimits config}
    #{fst $ aggregationLimits config} <= \#Aggregation2
    \#Aggregation2 <= #{upper $ aggregationLimits config}
    #{fst $ compositionLimits config} <= \#Composition2
    \#Composition2 <= #{upper $ compositionLimits config}
    #{fst $ inheritanceLimits config} <= \#Inheritance2
    \#Inheritance2 <= #{upper $ inheritanceLimits config}
    #{fst $ relationshipLimits config} <= \#Relationship2
    \#Relationship2 <= #{upper $ relationshipLimits config}
    #{fst $ classLimits config} <= \#Class
  }
}
|]
  where
    upper = fromMaybe (maxRels config) . snd

maybeToAlloySet :: Show a => Maybe a -> String
maybeToAlloySet = maybe "none" show

changes :: Maybe ClassConfig -> [RelationshipProperties] -> (Int, [String], String)
changes config propss = uncurry (length propss,,)
  $ snd $ foldl change (1, limits) propss
  where
    change (n, (cs, code)) p =
      let (c, code') = changeWithProperties p n
      in (n + 1, (c:cs, code ++ code'))
    limits = maybe ([], header) ((["changeLimits"],) . changeLimits) config
    header = [i|
//////////////////////////////////////////////////
// Changes
//////////////////////////////////////////////////
|]

changeWithProperties :: RelationshipProperties -> Int -> (String, String)
changeWithProperties props n = (change, alloy)
  where
    change = [i|change#{n}|]
    alloy =  [i|
sig C#{n} extends Change {}

pred #{change} {
  changeOfFirstCD [C#{n},
    #{wrongAssocs props}, #{wrongCompositions props}, #{selfRelationships props},
    #{selfInheritances props},
    #{maybeToAlloySet $ hasDoubleRelationships props},
    #{maybeToAlloySet $ hasReverseRelationships props},
    #{hasReverseInheritances props},
    #{maybeToAlloySet $ hasMultipleInheritances props},
    #{hasNonTrivialInheritanceCycles props},
    #{hasCompositionCycles props},
    #{maybeToAlloySet $ hasCompositionsPreventingParts props},
    #{maybeToAlloySet $ hasThickEdges props}]
}
|]

matchCdOdChanges :: ClassConfig -> Maybe Bool -> (Int, [String], String)
matchCdOdChanges config withNonTrivialInheritance =
  (3, ["changes", "changeLimits"],) $ [i|
//////////////////////////////////////////////////
// Changes
//////////////////////////////////////////////////
sig C1, C2, C3 extends Change {}

pred changes {
  one m1, m2 : Boolean {
    m1 = False or m2 = False
    let c1Assocs = Assoc - (Change.add - Assoc <: C1.add) - C1.remove,
        c2Assocs = Assoc - (Change.add - Assoc <: C2.add) - C2.remove {
      some c1Assocs or some c2Assocs
      let c1Inheritances = Inheritance - (Change.add - Inheritance <: C1.add) - C1.remove,
          c2Inheritances = Inheritance - (Change.add - Inheritance <: C2.add) - C2.remove {
        #{nonTrivialInheritanceConstraint "c1Inheritances" "c1Assocs" withNonTrivialInheritance}
        #{nonTrivialInheritanceConstraint "c2Inheritances" "c2Assocs" withNonTrivialInheritance}
      }
    }
    changeOfFirstCD [C1, 0, 0, 0, 0, False, False, False, False, False, False, False, m1]
    changeOfFirstCD [C2, 0, 0, 0, 0, False, False, False, False, False, False, False, m2]
    changeOfFirstCD [C3, 0, 0, 0, 0, False, False, False, False, False, False, False, False]
  }
}
|] ++ changeLimits config

changeLimits :: ClassConfig -> String
changeLimits config = [i|
pred changeLimits {
  all c : Change {
    let Association2 = Association - (Change.add - c.add) - c.remove,
        Composition2 = Composition - (Change.add - c.add) - c.remove,
        Aggregation2 = Aggregation - (Change.add - c.add) - c.remove,
        Inheritance2 = Inheritance - (Change.add - c.add) - c.remove {
      #{fst $ associationLimits config} <= \#Association2
      \#Association2 <= #{upper $ associationLimits config}
      #{fst $ aggregationLimits config} <= \#Aggregation2
      \#Aggregation2 <= #{upper $ aggregationLimits config}
      #{fst $ compositionLimits config} <= \#Composition2
      \#Composition2 <= #{upper $ compositionLimits config}
      #{fst $ inheritanceLimits config} <= \#Inheritance2
      \#Inheritance2 <= #{upper $ inheritanceLimits config}
    }
  }
}
|]
  where
    upper = fromMaybe (maxRels config) . snd

createRunCommand :: ClassConfig -> [String] -> Int ->  String
createRunCommand config@ClassConfig {..} predicates cs = [i|
run { #{command} } for #{rels} Relationship, #{bitSize} Int,
  #{exactClass}#{snd classLimits} Class, exactly #{cs} Change
|]
  where
    exactClass
      | uncurry (==) classLimits = "exactly "
      | otherwise            = ""
    relMax = fromMaybe (maxRels config) . snd $ relationshipLimits
    rels = relMax + cs
    bitSize :: Int
    bitSize = (+ 1) . ceiling @Double . logBase 2 . fromIntegral
      $ max rels (snd classLimits) + 1
    command :: String
    command = foldl ((++) . (++ " and ")) "cd" predicates

removeLines :: Int -> String -> String
removeLines n
  | n < 0     = id
  | otherwise = removeLines (n - 1) . removeLine

removeLine :: String -> String
removeLine = drop 1 . dropWhile (/= '\n')
