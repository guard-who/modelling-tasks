{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module CdAndChanges.Transform where

import Types
  (ClassConfig (..), RelationshipProperties (..))

import Data.FileEmbed                   (embedStringFile)
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (i)

transform :: ClassConfig -> RelationshipProperties -> String
transform config properties=
  removeLine $(embedStringFile "cd/assoclimits.als")
  ++ removeLines 3 $(embedStringFile "cd/generate.als")
  ++ classDiagram config properties
  ++ part
  ++ createRunCommand config predicates changes
  where
    (changes, predicates, part) = matchCdOdChanges config

maxRels :: ClassConfig -> Int
maxRels config = fromMaybe (maxClasses * (maxClasses - 1) `div` 2) $ sumOf4
  <$> snd (aggregations config)
  <*> snd (associations config)
  <*> snd (compositions config)
  <*> snd (inheritances config)
  where
    maxClasses = snd $ classes config
    sumOf4 w x y z = w + x + y + z

classDiagram :: ClassConfig -> RelationshipProperties -> String
classDiagram config props = [i|
//////////////////////////////////////////////////
// Basic CD
//////////////////////////////////////////////////
pred cd {
  let Assoc' = Assoc - Change.add,
      Association' = Association - Change.add,
      Aggregation' = Aggregation - Change.add,
      Composition' = Composition - Change.add,
      Relationship' = Relationship - Change.add,
      Inheritance' = Inheritance - Change.add {
    classDiagram [Assoc', Composition', Inheritance', Relationship',
      #{wrongAssocs props}, #{wrongCompositions props}, #{selfRelationships props},
      #{hasDoubleRelationships props}, #{hasReverseRelationships props},
      #{hasMultipleInheritances props}, #{hasInheritanceCycles props},
      #{hasCompositionCycles props}, #{maybeToAlloySet $ hasMarkedEdges props}]
    #{fst $ associations config} <= #Association'
    #Association' <= #{upper $ associations config}
    #{fst $ aggregations config} <= #Aggregation'
    #Aggregation' <= #{upper $ aggregations config}
    #{fst $ compositions config} <= #Composition'
    #Composition' <= #{upper $ compositions config}
    #{fst $ inheritances config} <= #Inheritance'
    #Inheritance' <= #{upper $ inheritances config}
    #{fst $ classes config} <= #Class
    3 <= #Relationship'
  }
}
|]
  where
    upper = fromMaybe (maxRels config) . snd
    maybeToAlloySet = maybe "none" show

matchCdOdChanges :: ClassConfig -> (Int, [String], String)
matchCdOdChanges config = (3, ["changes", "changeLimits"],) $ [i|
//////////////////////////////////////////////////
// Changes
//////////////////////////////////////////////////
sig C1, C2, C3 extends Change {}

pred changes {
  one m1, m2 : Boolean {
    m1 = False or m2 = False
    let c1Assocs = Assoc - (Change.add - Assoc <: C1.add) - C1.remove,
        c2Assocs = Assoc - (Change.add - Assoc <: C2.add) - C2.remove |
      some c1Assocs or some c2Assocs
    changeOfFirstCD [C1, 0, 0, 0, False, False, False, False, False, m1]
    changeOfFirstCD [C2, 0, 0, 0, False, False, False, False, False, m2]
    changeOfFirstCD [C3, 0, 0, 0, False, False, False, False, False, False]
  }
}
|] ++ changeLimits config

changeLimits :: ClassConfig -> String
changeLimits config = [i|
pred changeLimits {
  all c : Change {
    let Association' = Association - (Change.add - c.add) - c.remove,
        Composition' = Composition - (Change.add - c.add) - c.remove,
        Aggregation' = Aggregation - (Change.add - c.add) - c.remove,
        Inheritance' = Inheritance - (Change.add - c.add) - c.remove {
      #{fst $ associations config} <= #Association'
      #Association' <= #{upper $ associations config}
      #{fst $ aggregations config} <= #Aggregation'
      #Aggregation' <= #{upper $ aggregations config}
      #{fst $ compositions config} <= #Composition'
      #Composition' <= #{upper $ compositions config}
      #{fst $ inheritances config} <= #Inheritance'
      #Inheritance' <= #{upper $ inheritances config}
    }
  }
}
|]
  where
    upper = fromMaybe (maxRels config) . snd

createRunCommand :: ClassConfig -> [String] -> Int ->  String
createRunCommand config predicates changes = [i|
run { #{command} } for #{maxRels config + changes} Relationship,
  #{snd $ classes config} Class, #{changes} Change
|]
  where
    command :: String
    command = foldl ((++) . (++ " and ")) "cd" predicates

removeLines :: Int -> String -> String
removeLines n
  | n < 0     = id
  | otherwise = removeLines (n - 1) . removeLine

removeLine :: String -> String
removeLine = drop 1 . dropWhile (/= '\n')
