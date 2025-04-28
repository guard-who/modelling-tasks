{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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

also the following Alloy predicates, have been inlined

 * @ObjectFieldNames@
 * @ObjectLowerUpperAttribute@
 * @ObjectLowerAttribute@
 * @ObjectLowerUpper@
 * @ObjectLower@
 * @ObjectUpper@
 * @Composition@
-}
module Modelling.CdOd.CD2Alloy.Transform (
  ExtendsAnd (..),
  LinguisticReuse (..),
  Parts {- only for legacy-apps: -} (..),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  ) where

import qualified Data.List.NonEmpty.Extra         as NE (nubOrd, singleton)

import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectConfig (..),
  ObjectProperties (..),
  Relationship (..),
  relationshipName,
  )

import Data.Bifunctor                   (first)
import Data.Foldable                    (Foldable (toList), find)
import Data.Function                    ((&))
import Data.List                        ((\\), intercalate, union)
import Data.List.NonEmpty               (NonEmpty ((:|)))
import Data.List.Extra                  (nubOrd)
import Data.Maybe (
  catMaybes,
  fromMaybe,
  isJust,
  mapMaybe,
  )
import Data.String.Interpolate          (__i, i, iii)
import Data.Tuple.Extra                 ((&&&), dupe, uncurry3)

{-|
Parts belonging to the CD2Alloy Alloy program.
-}
data Parts = Parts {
  part1 :: !String,
  part2 :: !String,
  part3 :: !String
  }

{-|
To what degree, high-level Allow language features should be used
in order to express relationships directly.

Expressiveness is increased when relying less on Alloy language features.
Conciseness is increased when relying more on Alloy language features.
-}
data LinguisticReuse
  = None
  -- ^ not at all
  | ExtendsAnd !ExtendsAnd
  -- ^ use extends for inheritance
  deriving Show

{-|
What further to reuse than extends
-}
data ExtendsAnd
  = NothingMore
  -- ^ i.e. only extends
  | FieldPlacement
  -- ^ local field definition
  deriving Show

{-|
Generates Alloy code to generate object diagrams for a given class diagram.
The given class diagram must be valid otherwise the code generation
might not terminate.

The resulting code is split into parts which allows for recombination
with the generated Alloy code for other (similar) class diagrams.

(See 'mergeParts', 'combineParts' and 'createRunCommand')
-}
transform
  :: LinguisticReuse
  -- ^ the degree of linguistic reuse to apply during the translation
  -> Cd
  -- ^ the class diagram for which to generate the code
  -> Maybe [String]
  -- ^ all relationship names to consider
  -- (possibly across multiple class diagrams;
  -- if not provided, they will be taken from the provided class diagram)
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
  linguisticReuse
  ClassDiagram {classNames, relationships}
  maybeAllRelationshipNames
  abstractClassNames
  objectConfig
  ObjectProperties {..}
  index
  time =
  Parts { part1, part2, part3 }
  where
    allRelationshipNames = nubOrd $ fromMaybe
      (mapMaybe relationshipName relationships)
      maybeAllRelationshipNames
    part1 :: String
    part1 = [i|
// Alloy Model for CD#{index}
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: #{time}

module cd2alloy/CD#{index}Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////

#{objectDefinition}
#{objectsFact}
#{sizeConstraints}
#{loops}
#{inhabitance}
#{relationshipNameAppearance}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////
|]
    objectDefinition = case linguisticReuse of
      ExtendsAnd FieldPlacement -> ""
      _ -> [__i|
        //Parent of all classes relating fields and values
        abstract sig Object {
        #{fields}
        }
        |]
    fields = intercalate ",\n" $ map
      (\fieldName -> [i|  #{fieldName} : set Object|])
      allRelationshipNames
    objectsFact :: String
    objectsFact
      | hasLimitedIsolatedObjects
      = limitIsolatedObjects
      | otherwise
      = noEmptyInstances
    limitIsolatedObjects
      | null allRelationshipNames = [i|
fact LimitIsolatedObjects {
  #{false}
}
|]
      | otherwise = [i|
fact LimitIsolatedObjects {
 let get = #{alloySetOf allRelationshipNames} |
  \##{objectsParens} > mul[2, \#{o : #{objects} | no o.get and no get.o}]
}
|]
    noEmptyInstances = [i|
fact NonEmptyInstancesOnly {
  some #{objects}
}
|]
    (objects, objectsParens)
      | ExtendsAnd FieldPlacement <- linguisticReuse
      = alloySetOf &&& alloySetOfParens $ complete
      | otherwise = dupe "Object"
    complete = case linguisticReuse of
      None -> nonAbstractClassNames
      ExtendsAnd {} -> nonAbstractSuperClassNames
    nonAbstractSuperClassNames = nonAbstractClassNames \\ subClasses
    subClasses = (`mapMaybe` relationships) $ \case
        Inheritance {..} -> Just subClass
        _ -> Nothing
    withJusts f xs
      | any isJust xs = f $ catMaybes xs
      | otherwise     = ""
    sizeConstraints = withJusts (\ps -> [i|
fact SizeConstraints {
#{unlines ps}
}
|]) [
      ([i|  \##{objectsParens} >= |] ++) . show
        <$> maybeLower 1 (objectLimits objectConfig),
      "  " & uncurry count (first (maybeLow 0) $ linkLimits objectConfig),
      uncurry linksPerObjects
        $ first (maybeLow 0)
        $ linksPerObjectLimits objectConfig
      ]
    count = alloyCompare $ alloyPlus $ map ('#':) allRelationshipNames
    linksPerObjects :: Maybe Int -> Maybe Int -> Maybe String
    linksPerObjects maybeMin maybeMax =
      let maybeLinkCount = alloyPlus $ map
            (\link -> [i|plus[\#o.#{link}, minus[\##{link}.o, \#(o.#{link} & o)]]|])
            allRelationshipNames
      in [i|  all o : #{objects} | |] & alloyCompare maybeLinkCount maybeMin maybeMax
    maybeLower l = maybeLow l . fst
    maybeLow l x = if x <= l then Nothing else Just x
    part2 = [i|
// Classes
#{unlines (classSigs linguisticReuse relationships classNames)}
|] -- Figure 2.1, Rule 1, part 1
    part3 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Properties
#{predicate
  linguisticReuse
  index
  allRelationshipNames
  relationships
  complete
  }
|]
    nonAbstractClassNames = classNames \\ abstractClassNames
    nonAbstractObjects = case linguisticReuse of
      None -> nonAbstractClassNames
      ExtendsAnd {} -> (`map` nonAbstractClassNames) $ \name ->
        intercalate " - " $ name : directSubclassesOf relationships name
    inhabitance = case completelyInhabited of
      Nothing   -> ""
      Just False -> [i|
fact NotCompletelyInhabited {
  #{intercalate " or " $ map ("no " ++) nonAbstractObjects}
}|]
      Just True -> [i|
fact CompletelyInhabited {
#{unlines $ map ("  some " ++) nonAbstractObjects}
}|]
    relationshipNameAppearance = case usesEveryRelationshipName of
      Nothing -> ""
      Just False ->
        let notEvery = case allRelationshipNames of
              [] -> false
              names -> intercalate " or " $ map ("no " ++) names
        in [i|
fact UsesNotEveryRelationshipName {
  #{notEvery}
}|]
      Just True -> [i|
fact UsesEveryRelationshipName {
#{unlines $ map ("  some " ++) allRelationshipNames}
}|]
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just hasLoops
        | null allRelationshipNames, hasLoops -> [__i|
          fact SomeSelfLoops {
            #{false}
          }|]
        | null allRelationshipNames -> ""
        | otherwise ->
        if hasLoops
        then [i|
fact SomeSelfLoops {
  some o : #{objects} | o in o.#{relationshipSet}
}|]
        else [i|
fact NoSelfLoops {
  no o : #{objects} | o in o.#{relationshipSet}
}|]
    relationshipSet
      | [_] <- allRelationshipNames
      = alloySetOf allRelationshipNames
      | otherwise
      = "(" ++ alloySetOf allRelationshipNames ++ ")"

{-|
Generates Alloy code which is a contradiction that can never be satisfied,
i.e. essentially resolves to false.
-}
false :: String
false = "some none // i.e. false"

{-|
Given a possible counter formula and two limiters this function generates
Alloy code which is to be applied to an Alloy code prefix in order
to possible generate Alloy code that describes the resulting constraint.

(If the result is 'Nothing', nothing needs to be constrained)
-}
alloyCompare
  :: Maybe String
  -- ^ if 'Nothing' this counter formula is assumed to be equal to '0'
  -- (e.g. the case for the result of 'alloyPlus')
  -> Maybe Int
  -- ^ the lower limit
  -> Maybe Int
  -- ^ the upper limit
  -> String
  -- ^ the Alloy code prefix to prepend
  -> Maybe String
alloyCompare maybeWhat maybeMin maybeMax = case maybeWhat of
  Nothing
    | fromMaybe 0 maybeMin == 0 -> const Nothing
    | otherwise -> const $ Just $ "  " ++ false
  Just what -> case maybeMax of
    Nothing -> case maybeMin of
      Nothing -> const Nothing
      Just low -> Just . (++ [iii|#{what} >= #{low}|])
    Just up -> Just . case maybeMin of
      Nothing -> (++ [iii|#{what} =< #{up}|])
      Just low
        | low == up -> (++ [iii|#{what} = #{up}|])
        | otherwise ->
          (++ [iii|let count = #{what} | count >= #{low} and count =< #{up}|])

{-|
Creates an Alloy run command line taking provided size constraints into account.
-}
createRunCommand
  :: String
  -> Maybe [String]
  -- ^ if provided, object limit will be defined as constraint rather than
  -- by scope (by scope should be preferred, but is not possible for
  -- @ExtendsAnd FieldPlacement@)
  -> Int
  -> ObjectConfig
  -> [Relationship a b]
  -> String
createRunCommand
  command
  maybeNonAbstractClassNames
  numClasses
  objectConfig
  relationships
  = [i|
///////////////////////////////////////////////////
// Run commands
///////////////////////////////////////////////////
#{limitObjects}
run { #{command} } for #{maxObjects} #{what} #{intSize} Int
|]
  where
    maxLimit = maximum $ map maximumLimitOf relationships
    maxObjects = snd $ objectLimits objectConfig
    intSize :: Int
    intSize = ceiling intSize'
    intSize' :: Double
    intSize' = logBase 2 $ fromIntegral $ 2 * maxInt + 1
    maxInt = maximum [
      numClasses * maxObjects,
      maxLimit,
      2 * maxObjects,
      count linkLimits,
      count linksPerObjectLimits
      ]
    (what, limitObjects) = case maybeNonAbstractClassNames of
      Nothing -> ("Object,", "")
      Just nonAbstractClassNames -> (
        "but",
        [__i|
        fact objectsMaximum {
          \##{alloySetOfParens nonAbstractClassNames} <= #{maxObjects}
        }
        |]
        )
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

classSigs
  :: LinguisticReuse
  -> [Relationship String String]
  -> [String]
  -> [String]
classSigs linguisticReuse relationships = map classSig
  where
    classSig :: String -> String
    classSig name = case linguisticReuse of
      None -> [i|sig #{name} extends Object {}|]
      ExtendsAnd NothingMore ->
        (\super -> [i|sig #{name} extends #{super} {}|])
        $ maybe "Object" superClass
        $ find (hasSuperClass name) relationships
      ExtendsAnd FieldPlacement ->
        (\extends -> [__i|
          sig #{name}#{extends} #{fields name}#{spaced $ fieldConstraints name}|]
          )
        $ maybe "" ((" extends " ++) . superClass)
        $ find (hasSuperClass name) relationships
    fromTos = mapMaybe nameFromTo relationships
    fields className = linesWrappedInOrBraces $ commaSeparated $
      [ [iii|#{name} : set #{linking to}|]
      | (name, from, to) <- fromTos
      , linking from == className
      ]
    commaSeparated (x:xs@(_:_)) = (x ++ ",") : commaSeparated xs
    commaSeparated xs = xs
    spaced [] = []
    spaced xs = ' ':xs
    fieldConstraints className = linesWrappedInBraces $ filter (not . null) $
      [ nonInheritanceLimits (const name) (limits to) ""
      | (name, from, to) <- fromTos
      , linking from == className
      ] ++
      [ nonInheritanceLimits (const [iii|@#{name}.this|]) (limits from) ""
      | (name, from, to) <- fromTos
      , linking to == className
      ]
    hasSuperClass name = \case
      Inheritance {..} | subClass == name -> True
      _ -> False

{-| Puts curly braces arround the given lines which are indented,
puts a pair of empty curly braces if no lines are given, and
puts surrounding braces on that single line if just one line is given.
-}
linesWrappedInOrBraces :: [String] -> String
linesWrappedInOrBraces [] = "{}"
linesWrappedInOrBraces xs = linesWrappedInBraces xs

{-| Puts curly braces arround the given lines which are indented,
returns an empty string if no lines are given, and
puts surrounding braces on that single line if just one line is given.
-}
linesWrappedInBraces :: [String] -> String
linesWrappedInBraces [] = ""
linesWrappedInBraces [x] = [iii|{#{x}}|]
linesWrappedInBraces xs = [i|{
  #{intercalate "\n  " xs}
}|]

{-|
Retrieve the set of direct subclasses of the given class.
-}
directSubclassesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> String
  -- ^ the name of the class to consider
  -> [String]
directSubclassesOf relationships name = (`mapMaybe` relationships) $ \case
  Inheritance {superClass = s, ..} | name == s -> Just subClass
  _ -> Nothing

-- Figure 2.1, Rule 1, part 2, alternative implementation
-- (SubsCD - inlined)
{-|
Retrieve the set of all subclasses of the given class.
-}
allSubclassesOf
  :: [Relationship String String]
  -- ^ all relationships of the class diagram
  -> String
  -- ^ the name of the class to consider
  -> NonEmpty String
allSubclassesOf relationships name =
  NE.nubOrd
  $ name :| concatMap (toList . allSubclassesOf relationships) subclasses
  where
    subclasses = directSubclassesOf relationships name

{-|
The predicate constraining the specific class diagram.
-}
predicate
  :: LinguisticReuse
  -- ^ the degree of linguistic reuse to apply during the translation
  -> String
  -- ^ an identifier for the class diagram
  -> [String]
  -- ^ all relationship names to consider
  -- (possibly across multiple class diagrams)
  -> [Relationship String String]
  -- ^ all relationships belonging to the class diagram
  -> [String]
  -- ^ a complete set of class names that contains all possible objects
  -> String
predicate
  linguisticReuse
  index
  allRelationshipNames
  relationships
  complete
  = [i|
pred cd#{index} {
#{objects}
  // Contents
#{unlines nonExistingRelationships}
#{nonInheritanceConstraints linguisticReuse relationships}
  // Compositions
#{compositions}
}
|]
  where
    objects
      | ExtendsAnd FieldPlacement <- linguisticReuse = ""
      | otherwise = [i|
  Object = #{alloySetOf complete}
|]
      -- Figure 2.2, Rule 5
    objectSet
      | ExtendsAnd FieldPlacement <- linguisticReuse = alloySetOf complete
      | otherwise = "Object"
    nonExistingRelationships = map ("  no " ++)
      $ toList allRelationshipNames \\ mapMaybe relationshipName relationships
    compositions = compositeConstraint
      objectSet
      $ nubOrd $ flip mapMaybe relationships
      $ \case
        Composition {compositionName} -> Just compositionName
        _ -> Nothing
      -- Figure 2.2, Rule 4, corrected, simplified

nonInheritanceConstraints
  :: LinguisticReuse
  -> [Relationship String String]
  -> String
nonInheritanceConstraints linguisticReuse relationships
  | ExtendsAnd FieldPlacement <- linguisticReuse
  = ""
  | otherwise
  = [__i|
      // Associations
    #{unlines objectAttributes}
    |]
  where
    objectAttributes = concatMap
      (maybe [] (uncurry3 associationFromTo) . nameFromTo)
      relationships
    associationFromTo name from to = [
      makeNonInheritance (toList subsFrom) name (toList subsTo),
      makeNonInheritanceLimits (++ ('.' : name)) subsFrom (limits to),
      makeNonInheritanceLimits ((name ++) . ('.' :)) subsTo (limits from)
      ]
      where
        subsCd = case linguisticReuse of
          None -> allSubclassesOf relationships
          ExtendsAnd {} -> NE.singleton
        subsFrom = subsCd $ linking from
        subsTo = subsCd $ linking to

nameFromTo
  :: Relationship a b
  -> Maybe (b, LimitedLinking a, LimitedLinking a)
nameFromTo = \case
  Association {..} ->
    Just (associationName, associationFrom, associationTo)
  Aggregation {..} ->
    Just (aggregationName, aggregationPart, aggregationWhole)
  Composition {..} ->
    Just (compositionName, compositionPart, compositionWhole)
  Inheritance {} ->
    Nothing

{-|
Generates inlined Alloy code equivalent to a combination of the first parts of
the @ObjectLowerAttribute@ or @ObjectLowerUpperAttribute@
and the @ObjectFieldNames@ predicate.
-}
makeNonInheritance
  :: [String]
  -> String
  -> [String]
  -> String
makeNonInheritance fromSet name toSet = [i|
  #{name} in #{alloySetOfParens fromSet} -> #{alloySetOfParens toSet}|]
  -- alternative to @ObjectFieldNames@ predicate inlined and simplified

{-|
Generates inline, simplified Alloy code equivalent to
the @ObjectLowerAttribute@ or @ObjectLowerUpperAttribute@ predicate
or
the @ObjectLower@ or @ObjectLowerUpper@ predicate,
depending on the first parameter.
-}
makeNonInheritanceLimits
  :: (String -> String)
  -> NonEmpty String
  -> (Int, Maybe Int)
  -> String
makeNonInheritanceLimits nameLinking fromSet limits =
  [i|  all o : #{alloySetOf $ toList fromSet} | |]
  & nonInheritanceLimits nameLinking limits

{-|
Reusable part of defining multiplicity constraints for a non-inheritance
relationship.
-}
nonInheritanceLimits
  :: (String -> String)
  -> (Int, Maybe Int)
  -> String
  -- ^ String to prepend (if constraint needs to be set)
  -> String
nonInheritanceLimits nameLinking (low, maybeUp) =
  case maybeUp of
    Nothing -> case low of
      0 -> const ""
      _ -> (++ [iii|#{linkCount} >= #{low}|])
      -- @ObjectLowerAttribute@ or @ObjectLower@ predicate
      -- inlined and simplified
    Just up -> case low of
      0 -> (++ [iii|#{linkCount} =< #{up}|])
      _ | low == up -> (++ [iii|#{linkCount} = #{up}|])
        | otherwise -> (++ [iii|#{linkCount} >= #{low} and #{linkCount} =< #{up}|])
      -- @ObjectLowerUpperAttribute@ or @ObjectLowerUpper@ predicate
      -- inlined and simplified
  where
    linkCount = '#' : nameLinking "o"

{-|
Generates inlined, simplified Alloy code
equivalent to the @Composition@ predicate.
-}
compositeConstraint :: String -> [String] -> String
compositeConstraint objectSet nameSet =
  maybe ""
    (\usedFieldCount -> [i|  all o : #{objectSet} | #{usedFieldCount} =< 1|])
    $ alloyPlus
    $ (`map` toList nameSet) $ \fieldName ->
      [iii|\#o.#{fieldName}|]
  -- @Composition@ predicate inlined and simplified

{-|
Generates the code to add the Alloy Int values of the given list.
-}
alloyPlus :: [String] -> Maybe String
alloyPlus = \case
  [] -> Nothing
  [x] -> Just x
  xs@(_:_:_) -> alloyPlus $ pairPlus xs
  where
    pairPlus = \case
      [] -> []
      x@[_] -> x
      (x:y:zs) -> [i|plus[#{x}, #{y}]|] : pairPlus zs

{-|
Merges Alloy code 'Parts' for multiple class diagrams
to be used in a single Alloy query.
-}
mergeParts
  :: Parts
  -> Parts
  -> Parts
mergeParts p p' = Parts
  (part1 p)
  (part2 p `unionL` part2 p')
  (part3 p ++ part3 p')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y

{-|
Transforms 'Parts' into an Alloy program (besides the run command).

(See 'createRunCommand' for the latter)
-}
combineParts :: Parts -> String
combineParts Parts {..} = part1 ++ part2 ++ part3

{-|
Transform a set into Alloy code of a set.
-}
alloySetOf :: [String] -> String
alloySetOf xs
  | null xs = "none"
  | otherwise = intercalate " + " xs

alloySetOfParens :: [String] -> String
alloySetOfParens xs@(_:_:_) = [i|(#{alloySetOf xs})|]
alloySetOfParens xs = alloySetOf xs
