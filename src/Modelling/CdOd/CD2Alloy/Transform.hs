{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
This modules performs a Alloy code generation based on CD2Alloy
in order to generate object diagrams
based on (at least) one given class diagram using Alloy.

The whole transformation is based on the following paper
https://git.rwth-aachen.de/monticore/publications-additional-material/blob/master/cd2alloy/CD2AlloyTranslationTR.pdf
although a newer version of this document exists
https://www.se-rwth.de/publications/CD2Alloy-A-Translation-of-Class-Diagrams-to-Alloy.pdf

Throughout this module there are references to figures of the former paper,
indicating which part of the original work
the previous value definition is representing.

Also to increase readability, some identifiers, predicates, etc.
have been renamed opposed to the original work, these are:

Original: umlp2alloy ––– Here: cd2alloy
Original: FName ––– Here: FieldName
Original: fName ––– Here: fieldName
Original: Obj ––– Here: Object
Original: ObjUAttrib ––– Here: ObjectUpperAttribute
Original: ObjLAttrib ––– Here: ObjectLowerAttribute
Original: ObjLUAttrib ––– Here: ObjectLowerUpperAttribute
-}
module Modelling.CdOd.CD2Alloy.Transform (
  Parts {- only for legacy-apps: -} (..),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  ) where

import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectConfig (..),
  ObjectProperties (..),
  Relationship (..),
  relationshipName,
  )

import Data.Bifunctor                   (first, second)
import Data.List                        ((\\), intercalate, isPrefixOf, union)
import Data.FileEmbed                   (embedStringFile)
import Data.Maybe (
  catMaybes,
  fromMaybe,
  isJust,
  mapMaybe,
  )
import Data.String.Interpolate          (i)
import Data.Tuple.Extra                 (uncurry3)
import Polysemy.Plugin.Fundep.Utils     (singleListToJust)

{-|
Parts belonging to the CD2Alloy Alloy program.
-}
data Parts = Parts {
  part1 :: !String,
  part2 :: !String,
  part3 :: !String,
  part4 :: !String
  }

transform
  :: Cd
  -> [String]
  -> ObjectConfig
  -> ObjectProperties
  -> String
  -> String
  -> Parts
transform
  ClassDiagram {classNames, relationships}
  abstractClassNames
  objectConfig
  ObjectProperties {..}
  index
  time =
  Parts { part1, part2, part3, part4 }
  where
    template :: String
    template = $(embedStringFile "alloy/od/template.als")
    part1 :: String
    part1 = [i|
// Alloy Model for CD#{index}
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: #{time}

module cd2alloy/CD#{index}Module

#{template}
#{objectsFact}
#{sizeConstraints}
#{loops}
#{inhabitance}
#{relationshipNameAppearance}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////
|]
    objectsFact :: String
    objectsFact
      | hasLimitedIsolatedObjects
      = limitIsolatedObjects
      | otherwise
      = noEmptyInstances
    limitIsolatedObjects = [i|
fact LimitIsolatedObjects {
  \#Object > mul[2, \#{o : Object | no o.get and no get.o}]
}
|]
    noEmptyInstances = [i|
fact NonEmptyInstancesOnly {
  some Object
}
|]
    withJusts f xs
      | any isJust xs = f $ catMaybes xs
      | otherwise     = ""
    sizeConstraints = withJusts (\ps -> [i|
fact SizeConstraints {
#{unlines ps}
}
|]) [
      ("  #Object >= " ++) . show <$> maybeLower 1 (objectLimits objectConfig),
      ("  #get >= " ++) . show <$> maybeLower 0 (linkLimits objectConfig),
      ("  #get <= " ++) . show <$> snd (linkLimits objectConfig),
      uncurry linksPerObjects
        $ first (maybeLow 0)
        $ linksPerObjectLimits objectConfig
      ]
    linksPerObjects Nothing Nothing = Nothing
    linksPerObjects maybeMin maybeMax = Just $
      "  all o : Object | let x = plus[#o.get,minus[#get.o,#o.get.o]] |"
      ++ maybe "" ((" x >= " ++) . show) maybeMin
      ++ maybe "" (const " &&") (maybeMin >> maybeMax)
      ++ maybe "" ((" x <= " ++) . show) maybeMax
    maybeLower l = maybeLow l . fst
    maybeLow l x = if x <= l then Nothing else Just x
    part2 = [i|
// Concrete names of fields
#{unlines (associationSigs relationships)}
|] -- Figure 2.1, Rule 3, part 2
    part3 = [i|
// Classes (non-abstract)
#{unlines (classSigs nonAbstractClassNames)}
|] -- Figure 2.1, Rule 1, part 1
    part4 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Types wrapping subtypes
#{typeHierarchy}
// Types wrapping field names
#{fields}
// Types wrapping composite structures and field names
#{if noCompositions then "" else compositeStructures}
// Properties
#{predicate index relationships nonAbstractClassNames}
|]
    nonAbstractClassNames = classNames \\ abstractClassNames
    noCompositions = all (\case Composition {} -> False; _ -> True) relationships
    typeHierarchy =
      unlines (subTypes index relationships abstractClassNames classNames)
      -- Figure 2.1, Rule 1, part 2, alternative implementation
    fields =
      unlines (fieldNames index relationships classNames)
      -- Figure 2.2, Rule 2, relevant portion, alternative implementation
    compositeStructures =
      unlines (compositesAndFieldNames index relationships classNames)
      -- Figure 2.1, Rule 6, corrected
    inhabitance = case completelyInhabited of
      Nothing   -> ""
      Just False -> [i|
fact NotCompletelyInhabited {
  #{intercalate " or " $ map ("no " ++) nonAbstractClassNames}
}|]
      Just True -> [i|
fact CompletelyInhabited {
#{unlines $ map ("  some " ++) nonAbstractClassNames}
}|]
    relationshipNameAppearance = case usesEveryRelationshipName of
      Nothing -> ""
      Just False -> [i|
fact UsesNotEveryRelationshipName {
  #{intercalate " or " $ map ("no " ++) namesLinkingTo}
}|]
      Just True -> [i|
fact UsesEveryRelationshipName {
#{unlines $ map ("  some " ++) namesLinkingTo}
}|]
    namesLinkingTo = mapMaybe
      (fmap (\name -> "Object.get[" ++ name ++ "]") . relationshipName)
      relationships
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just True  -> [i|
fact SomeSelfLoops {
  some o : Object | o in o.get[FieldName]
}|]
      Just False -> [i|
fact NoSelfLoops {
  no o : Object | o in o.get[FieldName]
}|]

hasLinkNames :: Parts -> Bool
hasLinkNames Parts { part2 } =
  any (oneSig `isPrefixOf`) $ lines part2

createRunCommand
  :: String
  -> Int
  -> ObjectConfig
  -> [Relationship a b]
  -> Parts
  -> String
createRunCommand command numClasses objectConfig relationships ps = [i|
///////////////////////////////////////////////////
// Run commands
///////////////////////////////////////////////////

run { #{command} } for #{fieldNamesLimit}#{maxObjects} Object, #{intSize} Int
|]
  where
    maxLimit = maximum $ map maximumLimitOf relationships
    maxObjects = snd $ objectLimits objectConfig
    intSize :: Int
    intSize = ceiling intSize'
    intSize' :: Double
    intSize' = logBase 2 $ fromIntegral $ 2 * maxInt + 1
    fieldNamesLimit
      | hasLinkNames ps = "" :: String
      | otherwise       = "0 FieldName, "
    maxInt = maximum [
      numClasses * maxObjects,
      maxLimit,
      2 * maxObjects,
      count linkLimits,
      count linksPerObjectLimits
      ]
    count f = fromMaybe (fst $ f objectConfig) $ snd (f objectConfig)

maximumLimitOf :: Relationship a b -> Int
maximumLimitOf = \case
  Association {..} -> maximumLimit associationFrom associationTo
  Aggregation {..} -> maximumLimit aggregationPart aggregationWhole
  Composition {..} -> maximumLimit compositionPart compositionWhole
  Inheritance {}   -> 0
  where
    maximumLimit l1 l2 = max (maximumLinking l1) (maximumLinking l2)
    maximumLinking LimitedLinking {limits = (low, high)} = fromMaybe low high

oneSig :: String
oneSig = "one sig "

associationSigs :: [Relationship c String] -> [String]
associationSigs = mapMaybe
  $ fmap (\name -> oneSig ++ name ++ " extends FieldName {}") . relationshipName


classSigs :: [String] -> [String]
classSigs = map (\name -> "sig " ++ name ++ " extends Object {}")

subTypes
  :: String
  -> [Relationship String String]
  -> [String]
  -> [String]
  -> [String]
subTypes index rs abstractClassNames = concatMap $ \name ->
  [ "fun " ++ name ++ subsCD ++ " : set Object {"
  , "  " ++ intercalate " + " ((if name `elem` abstractClassNames then "none" else name)
                               : map (++ subsCD) (directSubclassesOf name))
  , "}"
  ]
  where
    subsCD = "SubsCD" ++ index
    directSubclassesOf x = (`mapMaybe` rs) $ \case
      Inheritance { superClass = s, .. } | x == s -> Just subClass
      _ -> Nothing

fieldNames
  :: String
  -> [Relationship String String]
  -> [String]
  -> [String]
fieldNames index relationships = concatMap $ \this ->
  let (superClasses, associationNames) = relationshipsFrom this
  in [ "fun " ++ this ++ fieldNamesCD ++" : set FieldName {"
     , "  " ++ intercalate
         " + "
         (maybe "none" (++ fieldNamesCD) (singleListToJust superClasses)
           : associationNames)
     , "}"
     ]
  where
    fieldNamesCD = "FieldNamesCD" ++ index
    relationshipsFrom x =
      foldr (addRelationship x) ([], []) relationships
    addRelationship from c = case c of
      AssociationFrom x | from == x -> second (associationName c :)
                        | otherwise -> id
      AggregationFrom x | from == x -> second (aggregationName c :)
                        | otherwise -> id
      CompositionFrom x | from == x -> second (compositionName c :)
                        | otherwise -> id
      Inheritance {..} | from == subClass -> first (superClass :)
                       | otherwise -> id

{-# COMPLETE AssociationFrom, AggregationFrom, CompositionFrom, Inheritance #-}
{-# COMPLETE Association, Aggregation, CompositionTo, Inheritance #-}

pattern AssociationFrom :: className -> Relationship className relationshipName
pattern AssociationFrom from <- Association {
  associationFrom = LimitedLinking { linking = from }
  }

pattern AggregationFrom :: className -> Relationship className relationshipName
pattern AggregationFrom from <- Aggregation {
  aggregationWhole = LimitedLinking { linking = from }
  }

pattern CompositionFrom :: className -> Relationship className relationshipName
pattern CompositionFrom from <- Composition {
  compositionWhole = LimitedLinking { linking = from }
  }

pattern CompositionTo :: className -> Relationship className relationshipName
pattern CompositionTo to <- Composition {
  compositionPart = LimitedLinking { linking = to }
  }

compositesAndFieldNames
  :: String
  -> [Relationship String String]
  -> [String]
  -> [String]
compositesAndFieldNames index relationships = concatMap $ \this ->
  let (superClasses, compositions) = supersAndCompositionsOf this
      super = singleListToJust superClasses
  in [ "fun " ++ this ++ compositesCD ++ " : set Object {"
     , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\c -> whole c ++ subsCD) compositions)
     , "}"
     , "fun " ++ this ++ compFieldNamesCD ++ " : set FieldName {"
     , "  " ++ intercalate " + " (maybe "none" (++ compFieldNamesCD) super
                                  : map compositionName compositions)
     , "}"
     ]
  where
    whole = linking . compositionWhole
    compositesCD = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD = "SubsCD" ++ index
    supersAndCompositionsOf x =
      foldr (addSuperOrComposition x) ([], []) relationships
    addSuperOrComposition here c = case c of
      Association {} -> id
      Aggregation {} -> id
      CompositionTo x | here == x -> second (c :)
                      | otherwise -> id
      Inheritance {..} | here == subClass -> first (superClass :)
                       | otherwise -> id

predicate :: String -> [Relationship String String] -> [String] -> String
predicate index relationships nonAbstractClassNames = [i|
pred cd#{index} {

  #{objects}

  // Contents
#{unlines objectFieldNames}
  // Associations
#{unlines objectAttributes}
  // Compositions
#{if anyCompositions then unlines compositions else ""}
}
|]
  where
    objects =
      "Object = " ++ intercalate " + " ("none" : nonAbstractClassNames)
      -- Figure 2.2, Rule 5
    objectFieldNames = map
      (\name -> [i|  ObjectFieldNames[#{name}, #{name}#{fieldNamesCD}]|])
      nonAbstractClassNames
      -- Figure 2.3, Rule A3
    nameFromTo = \case
      Association {..} ->
        Just (associationName, associationFrom, associationTo)
      Aggregation {..} ->
        Just (aggregationName, aggregationWhole, aggregationPart)
      Composition {..} ->
        Just (compositionName, compositionWhole, compositionPart)
      Inheritance {} ->
        Nothing
    objectAttributes = concatMap
      (maybe [] (uncurry3 associationFromTo) . nameFromTo)
      relationships
    associationFromTo name from to = [
      makeNonInheritance "Attribute" (linking from) name (linking to) (limits to),
      makeNonInheritance "" (linking to) name (linking from) (limits from)
      ]
    makeNonInheritance
      :: Show a
      => String -> String -> String -> String -> (a, Maybe a) -> String
    makeNonInheritance att from name to (low, Nothing) =
      [i|  ObjectLower#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}]|]
    makeNonInheritance att from name to (low, Just up) =
      [i|  ObjectLowerUpper#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}, #{show up}]|]
    anyCompositions =
      any (\case Composition {} -> True; _ -> False) relationships
    compositions = map
      (\name -> [i|  Composition[#{name}#{compositesCD}, #{name}#{compFieldNamesCD}, #{name}]|])
      nonAbstractClassNames
      -- Figure 2.2, Rule 4, corrected
    fieldNamesCD     = "FieldNamesCD" ++ index
    compositesCD     = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD           = "SubsCD" ++ index

mergeParts
  :: Parts
  -> Parts
  -> Parts
mergeParts p p' = Parts
  (part1 p)
  (part2 p `unionL` part2 p')
  (part3 p `unionL` part3 p')
  (part4 p ++ part4 p')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y

combineParts :: Parts -> String
combineParts Parts {..} = part1 ++ part2 ++ part3 ++ part4
