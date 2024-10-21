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
  union,
  unions,
  )

import Data.Bifunctor                   (first, second)
import Data.Foldable                    (Foldable (toList))
import Data.Function                    ((&))
import Data.List                        ((\\), intercalate, union)
import Data.Maybe (
  catMaybes,
  fromMaybe,
  isJust,
  mapMaybe,
  maybeToList,
  )
import Data.Set                         (Set)
import Data.String.Interpolate          (i, iii)
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
  -> Maybe (Set String)
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
  ClassDiagram {classNames, relationships}
  maybeAllRelationshipNames
  abstractClassNames
  objectConfig
  ObjectProperties {..}
  index
  time =
  Parts { part1, part2, part3, part4 }
  where
    allRelationshipNames = fromMaybe
      (S.fromList $ mapMaybe relationshipName relationships)
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

//Parent of all classes relating fields and values
abstract sig Object {
#{fields}
}
#{objectsFact}
#{sizeConstraints}
#{loops}
#{inhabitance}
#{relationshipNameAppearance}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////
|]
    fields = intercalate ",\n" $ map
      (\fieldName -> [i|  #{fieldName} : set Object|])
      allRelationshipsList
    allRelationshipsList = toList allRelationshipNames
    objectsFact :: String
    objectsFact
      | hasLimitedIsolatedObjects
      = limitIsolatedObjects
      | otherwise
      = noEmptyInstances
    limitIsolatedObjects = [i|
fact LimitIsolatedObjects {
 let get = #{allRelationships} |
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
      fmap ("  " ++) . count
        $ maybeToList ((" >= " ++) . show <$> maybeLower 0 (linkLimits objectConfig))
        ++ maybeToList ((" <= " ++) . show <$> snd (linkLimits objectConfig)),
      uncurry linksPerObjects
        $ first (maybeLow 0)
        $ linksPerObjectLimits objectConfig
      ]
    counted = alloyPlus $ map ('#':) allRelationshipsList
    count [] = Nothing
    count [x] = Just $ counted ++ x
    count (x:y:_) = Just [iii|let count = #{counted} | count#{x} and count#{y}|]
    linksPerObjects Nothing Nothing = Nothing
    linksPerObjects maybeMin maybeMax = Just $
      "  all o : Object | "
      ++ [iii|
        let x = plus[#{alloyPlus $ map ("#o." ++) allRelationshipsList},
          minus[#{alloyPlus $ map (\get -> '#' : get ++ ".o") allRelationshipsList},
           #{alloyPlus $ map (\get -> "#(o." ++ get ++ " & o)") allRelationshipsList}]] |
        |]
      ++ maybe "" ((" x >= " ++) . show) maybeMin
      ++ maybe "" (const " &&") (maybeMin >> maybeMax)
      ++ maybe "" ((" x <= " ++) . show) maybeMax
    maybeLower l = maybeLow l . fst
    maybeLow l x = if x <= l then Nothing else Just x
    part2 = "" -- Figure 2.1, Rule 3, part 2
    part3 = [i|
// Classes (non-abstract)
#{unlines (classSigs classNames)}
|] -- Figure 2.1, Rule 1, part 1
    part4 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Properties
#{predicate index allRelationshipNames relationships nonAbstractClassNames}
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
    namesLinkingTo = map ("Object." ++) allRelationshipsList
    allRelationships = alloySetOf allRelationshipNames
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just hasLoops -> withAlloyJoin ["o"] allRelationshipNames $ \join ->
        if hasLoops
        then [i|
fact SomeSelfLoops {
  some o : Object | o in #{join}
}|]
        else [i|
fact NoSelfLoops {
  no o : Object | o in #{join}
}|]

{-|
Creates an Alloy run command line taking provided size constraints into account.
-}
createRunCommand
  :: String
  -> Int
  -> ObjectConfig
  -> [Relationship a b]
  -> String
createRunCommand command numClasses objectConfig relationships = [i|
///////////////////////////////////////////////////
// Run commands
///////////////////////////////////////////////////

run { #{command} } for #{maxObjects} Object, #{intSize} Int
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
  -> String
  -- ^ the name of the class to consider
  -> Set String
allSubclassesOf relationships name =
  S.insert name
  $ S.unions $ map (allSubclassesOf relationships) subclasses
  where
    subclasses = (`mapMaybe` relationships) $ \case
      Inheritance {superClass = s, ..} | name == s -> Just subClass
      _ -> Nothing

{-# COMPLETE Association, Aggregation, CompositionTo, Inheritance #-}

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
  -> String
  -- ^ the name of the class to consider
  -> Set String
allCompositionFieldNamesOf relationships name =
  maybeUnion (allCompositionFieldNamesOf relationships) super
  $ S.fromList $ map compositionName compositions
  where
    (super, compositions) = superAndCompositionsOf relationships name

{-|
The predicate constraining the specific class diagram.
-}
predicate
  :: String
  -- ^ an identifier for the class diagram
  -> Set String
  -- ^ all relationship names to consider
  -- (possibly across multiple class diagrams)
  -> [Relationship String String]
  -- ^ all relationships belonging to the class diagram
  -> [String]
  -- ^ the set of non abstract class names
  -> String
predicate index allRelationshipNames relationships nonAbstractClassNames = [i|
pred cd#{index} {

  #{objects}

  // Contents
#{unlines nonExistingRelationships}
  // Associations
#{unlines objectAttributes}
  // Compositions
#{unlines compositions}
}
|]
  where
    objects =
      "Object = " ++ intercalate " + " ("none" : nonAbstractClassNames)
      -- Figure 2.2, Rule 5
    nonExistingRelationships = map ("  no " ++)
      $ toList allRelationshipNames \\ mapMaybe relationshipName relationships
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
      makeNonInheritanceAttribute subsFrom name subsTo (limits to),
      makeNonInheritance subsTo name (limits from)
      ]
      where
        subsFrom = subsCd $ linking from
        subsTo = subsCd $ linking to
    compositions = mapMaybe
      (\to -> ("  " ++)
        <$> compositeConstraint (compFieldNamesCd to) to)
      nonAbstractClassNames
      -- Figure 2.2, Rule 4, corrected
    compFieldNamesCd = allCompositionFieldNamesOf relationships
    subsCd = allSubclassesOf relationships

{-|
Generates inlined Alloy code equivalent to the @ObjectLowerAttribute@
or @ObjectLowerUpperAttribute@ predicate.
-}
makeNonInheritanceAttribute
  :: Set String
  -> String
  -> Set String
  -> (Int, Maybe Int)
  -> String
makeNonInheritanceAttribute fromSet name toSet (low, maybeUp) =
  (++) [i|  #{name}.Object in #{fromAlloySet}
  Object.#{name} in #{alloySetOf toSet}|]
  -- alternative to @ObjectFieldNames@ predicate inlined and simplified
  $ ('\n' : [i|  all o : #{fromAlloySet} | |])
  & case maybeUp of
    Nothing -> case low of
      0 -> const ""
      _ -> (++ [iii|\#o.#{name} >= #{low}|])
      -- @ObjectLowerAttribute@ predicate inlined
    Just up -> case low of
      0 -> (++ [iii|\#o.#{name} =< #{up}|])
      _ | low == up -> (++ [iii|\#o.#{name} = #{up}|])
        | otherwise -> (++ [iii|let n = \#o.#{name} | n >= #{low} and n =< #{up}|])
      -- @ObjectLowerUpperAttribute@ predicate inlined
  where
    fromAlloySet = alloySetOf fromSet

{-|
Generates inlined Alloy code equivalent to the @ObjectLowerUpper@ predicate.
-}
makeNonInheritance
  :: Set String
  -> String
  -> (Int, Maybe Int)
  -> String
makeNonInheritance fromSet name (low, maybeUp) =
  [i|  all o : #{alloySetOf fromSet} | |]
  & case maybeUp of
    Nothing -> case low of
      0 -> const ""
      _ -> (++ [iii|\##{name}.o >= #{low}|])
      -- @ObjectLower@ predicate inlined and simplified
    Just up -> case low of
      0 -> (++ [iii|\##{name}.o =< #{up}|])
      _ | low == up -> (++ [iii|\##{name}.o = #{up}|])
        | otherwise -> flip (++) [iii|
        let n = \##{name}.o |
          n >= #{low} and n =< #{up}
        |]
      -- @ObjectLowerUpper@ predicate inlined and simplified

{-|
Generates inlined Alloy code equivalent to the @Composition@ predicate.
-}
compositeConstraint :: Set String -> String -> Maybe String
compositeConstraint nameSet to
  | null nameSet = Nothing
  | otherwise = Just $
    (\usedFieldCount -> [i|all o : #{to} | #{usedFieldCount} =< 1|])
    $ alloyPlus
    $ (`map` toList nameSet) $ \fieldName ->
      [iii|\##{fieldName}.o|]
  -- @Composition@ predicate inlined

{-|
Generates the code to add the Alloy Int values of the given list.
-}
alloyPlus :: [String] -> String
alloyPlus = \case
  [] -> "0"
  [x] -> x
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
  (part3 p `unionL` part3 p')
  (part4 p ++ part4 p')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y

{-|
Transforms 'Parts' into an Alloy program (besides the run command).

(See 'createRunCommand' for the latter)
-}
combineParts :: Parts -> String
combineParts Parts {..} = part1 ++ part2 ++ part3 ++ part4

{-|
Transform a 'Foldable structure' into Alloy code of a set.
-}
alloySetOf :: Foldable f => f String -> String
alloySetOf xs
  | null xs = "none"
  | otherwise = intercalate " + " $ toList xs

{-|
Generate code for joining two sets (but return Nothing if either is empty).

Uses 'alloySetOf' in order to transform the sets into alloySets.
-}
maybeAlloyJoin
  :: (Foldable f1, Foldable f2)
  => f1 String
  -> f2 String
  -> Maybe String
maybeAlloyJoin xs ys
  | null xs || null ys = Nothing
  | otherwise = Just $ alloySet xs ++ '.' : alloySet ys
  where
    alloySet zs =
      if length zs == 1
      then alloySetOf zs
      else "(" ++ alloySetOf zs ++ ")"

{-|
Generate Alloy code to join two sets and process it using the given function.

(uses 'maybeAlloyJoin')
-}
withAlloyJoin
  :: (Foldable f1, Foldable f2)
  => f1 String
  -> f2 String
  -> (String -> String)
  -> String
withAlloyJoin xs ys f = maybe "" f (maybeAlloyJoin xs ys)
