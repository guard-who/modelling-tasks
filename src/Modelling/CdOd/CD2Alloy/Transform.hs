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
<https://git.rwth-aachen.de/monticore/publications-additional-material/blob/master/cd2alloy/CD2AlloyTranslationTR.pdf>
although a newer version of this document exists
<https://www.se-rwth.de/publications/CD2Alloy-A-Translation-of-Class-Diagrams-to-Alloy.pdf>

Throughout this module there are references to figures of the former paper,
indicating which part of the original work
the previous value definition is representing.

Also to increase readability, some identifiers, predicates, etc.
have been renamed opposed to the original work, these are:

@
Original: umlp2alloy ––– Here: cd2alloy
Original: FName ––– Here: FieldName
Original: fName ––– Here: fieldName
Original: Obj ––– Here: Object
Original: ObjUAttrib ––– Here: ObjectUpperAttribute
Original: ObjLAttrib ––– Here: ObjectLowerAttribute
Original: ObjLUAttrib ––– Here: ObjectLowerUpperAttribute
@

Furthermore refactorings have been made which inline Alloy functions, these are

 * @...CompFieldNamesCD...@
 * @...CompositesCD...@
 * @...FieldNamesCD...@
 * @...SubsCD...@
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

import qualified Data.Set                         as S (
  fromList,
  insert,
  null,
  toList,
  union,
  unions,
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
import Data.Set                         (Set)
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

{-|
Generates Alloy code to generate object diagrams for a given class diagram.
The given class diagram must be valid otherwise the code generation
might not terminate.

The resulting code is split into parts which allows for recombination
with the generated Alloy code for other (similar) class diagrams.

(See 'mergeParts', 'combineParts' and 'createRunCommand')
-}
transform
  :: Cd
  -- ^ the class diagram for which to generate the code
  -> [String]
  -- ^ the list of abstract class names
  -> ObjectConfig
  -- ^ size constraints on the object diagrams
  -> ObjectProperties
  -- ^ structural constraints for the object diagrams
  -> String
  -- ^ an identifier for the class diagram
  -> String
  -- ^ a time stamp
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

// Properties
#{predicate index relationships abstractClassNames nonAbstractClassNames}
|]
    nonAbstractClassNames = classNames \\ abstractClassNames
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

-- Figure 2.1, Rule 1, part 2, alternative implementation
-- (SubsCD - inlined)
{-|
Retrieve the set of all subclasses of the given class.
-}
allSubclassesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> [String]
  -- ^ the set of abstract class names
  -> String
  -- ^ the name of the class to consider
  -> Set String
allSubclassesOf relationships abstractClassNames name =
  (if name `elem` abstractClassNames then id else S.insert name)
  $ S.unions $ map (allSubclassesOf relationships abstractClassNames) subclasses
  where
    subclasses = (`mapMaybe` relationships) $ \case
      Inheritance {superClass = s, ..} | name == s -> Just subClass
      _ -> Nothing

-- Figure 2.2, Rule 2, relevant portion, alternative implementation
-- (FieldNamesCD - inlined)
{-|
Retrieve the set of all field names of the given class.
-}
allFieldNamesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> String
  -- ^ the name of the class to consider
  -> Set String
allFieldNamesOf relationships name =
  maybeUnion (allFieldNamesOf relationships) (singleListToJust superClasses)
  $ S.fromList associationNames
  where
    (superClasses, associationNames) = relationshipsFrom name
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

{-|
Retrieves the direct superclass and composites of the given class.
-}
superAndCompositionsOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> String
  -- ^ the name of the class to consider
  -> (Maybe String, [Relationship String String])
superAndCompositionsOf relationships name =
  first singleListToJust
  $ foldr addSuperOrComposition ([], []) relationships
  where
    addSuperOrComposition c = case c of
      Association {} -> id
      Aggregation {} -> id
      CompositionTo x
        | name == x -> second (c :)
        | otherwise -> id
      Inheritance {..}
        | name == subClass -> first (superClass :)
        | otherwise -> id

-- Figure 2.1, Rule 6, corrected
-- (CompositesCD - inlined)
{-|
Retrieve the set of all composites of the given class.
-}
allCompositesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> [String]
  -- ^ the set of abstract class names
  -> String
  -- ^ the name of the class to consider
  -> Set String
allCompositesOf relationships abstractClassNames name =
  maybeUnion (allCompositesOf relationships abstractClassNames) super
  $ S.unions
  $ map (allSubclassesOf relationships abstractClassNames . whole) compositions
  where
    whole = linking . compositionWhole
    (super, compositions) = superAndCompositionsOf relationships name

maybeUnion :: Ord b => (a -> Set b) -> Maybe a -> Set b -> Set b
maybeUnion f = maybe id (S.union . f)

-- Figure 2.1, Rule 6, corrected
-- (CompFieldNamesCD - inlined)
{-|
Retrieve the set of all composites of the given class.
-}
allCompositionFieldNamesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> [String]
  -- ^ the set of abstract class names
  -> String
  -- ^ the name of the class to consider
  -> Set String
allCompositionFieldNamesOf relationships abstractClassNames name =
  maybeUnion (allCompositionFieldNamesOf relationships abstractClassNames) super
  $ S.fromList $ map compositionName compositions
  where
    (super, compositions) = superAndCompositionsOf relationships name

predicate
  :: String
  -> [Relationship String String]
  -> [String]
  -> [String]
  -> String
predicate index relationships abstractClassNames nonAbstractClassNames = [i|
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
      (\name -> [i|  ObjectFieldNames[#{name}, #{fieldNamesCd name}]|])
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
      [i|  ObjectLower#{att}[#{subsCd from}, #{name}, #{subsCd to}, #{show low}]|]
    makeNonInheritance att from name to (low, Just up) =
      [i|  ObjectLowerUpper#{att}[#{subsCd from}, #{name}, #{subsCd to}, #{show low}, #{show up}]|]
    anyCompositions =
      any (\case Composition {} -> True; _ -> False) relationships
    compositions = map
      (\name -> [i|  Composition[#{compositesCd name}, #{compFieldNamesCd name}, #{name}]|])
      nonAbstractClassNames
      -- Figure 2.2, Rule 4, corrected
    fieldNamesCd = alloySetOf . allFieldNamesOf relationships
    compositesCd = alloySetOf . allCompositesOf relationships abstractClassNames
    compFieldNamesCd = alloySetOf
      . allCompositionFieldNamesOf relationships abstractClassNames
    subsCd = alloySetOf . allSubclassesOf relationships abstractClassNames

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

{-|
Transform a 'Set' into Alloy code of a set.
-}
alloySetOf :: Set String -> String
alloySetOf xs
  | S.null xs = "none"
  | otherwise = intercalate " + " $ S.toList xs
