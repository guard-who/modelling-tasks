{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
  -> Maybe Bool
  -> Bool
  -> String
  -> String
  -> Parts
transform
  ClassDiagram {classNames, relationships}
  abstractClassNames
  objectConfig
  hasSelfLoops
  noIsolationLimitation
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

module umlp2alloy/CD#{index}Module

#{template}
#{objectsFact}
#{sizeConstraints}
#{loops}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////
|]
    objectsFact :: String
    objectsFact
      | noIsolationLimitation
      = noEmptyInstances
      | otherwise
      = limitIsolatedObjects
    limitIsolatedObjects = [i|
fact LimitIsolatedObjects {
  \#Obj > mul[2, \#{o : Obj | no o.get and no get.o}]
}
|]
    noEmptyInstances = [i|
fact NonEmptyInstancesOnly {
  some Obj
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
      ("  #Obj >= " ++) . show <$> mlower 1 (objectLimits objectConfig),
      ("  #get >= " ++) . show <$> mlower 0 (linkLimits objectConfig),
      ("  #get <= " ++) . show <$> snd (linkLimits objectConfig),
      uncurry linksPerObjects
        $ first (mlow 0)
        $ linksPerObjectLimits objectConfig
      ]
    linksPerObjects Nothing Nothing = Nothing
    linksPerObjects mmin mmax = Just $
      "  all o : Obj | let x = plus[#o.get,minus[#get.o,#o.get.o]] |"
      ++ maybe "" ((" x >= " ++) . show) mmin
      ++ maybe "" (const " &&") (mmin >> mmax)
      ++ maybe "" ((" x <= " ++) . show) mmax
    mlower l = mlow l . fst
    mlow l x = if x <= l then Nothing else Just x
    part2 = [i|
// Concrete names of fields
#{unlines (associationSigs relationships)}
|]
    part3 = [i|
// Classes (non-abstract)
#{unlines (classSigs nonAbstractClassNames)}
|]
    part4 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Types wrapping subtypes
#{unlines (subTypes index relationships abstractClassNames classNames)}
// Types wrapping field names
#{unlines (fieldNames index relationships classNames)}
// Types wrapping composite structures and field names
#{if noCompositions then "" else compositeStructures}
// Properties
#{predicate index relationships nonAbstractClassNames}
|]
    nonAbstractClassNames = classNames \\ abstractClassNames
    noCompositions = all (\case Composition {} -> False; _ -> True) relationships
    compositeStructures =
      unlines (compositesAndFieldNames index relationships classNames)
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just True  -> [i|
fact SomeSelfLoops {
  some o : Obj | o in o.get[FName]
}|]
      Just False -> [i|
fact NoSelfLoops {
  no o : Obj | o in o.get[FName]
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

run { #{command} } for #{fnames}#{maxObjects} Obj, #{intSize} Int
|]
  where
    maxLimit = maximum $ map maximumLimitOf relationships
    maxObjects = snd $ objectLimits objectConfig
    intSize :: Int
    intSize = ceiling intSize'
    intSize' :: Double
    intSize' = logBase 2 $ fromIntegral $ 2 * maxInt + 1
    fnames
      | hasLinkNames ps = "" :: String
      | otherwise       = "0 FName, "
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
  $ fmap (\name -> oneSig ++ name ++ " extends FName {}") . relationshipName


classSigs :: [String] -> [String]
classSigs = map (\name -> "sig " ++ name ++ " extends Obj {}")

subTypes
  :: String
  -> [Relationship String String]
  -> [String]
  -> [String]
  -> [String]
subTypes index rs abstractClassNames = concatMap $ \name ->
  [ "fun " ++ name ++ subsCD ++ " : set Obj {"
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
  in [ "fun " ++ this ++ fieldNamesCD ++" : set FName {"
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
  in [ "fun " ++ this ++ compositesCD ++ " : set Obj {"
     , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\c -> whole c ++ subsCD) compositions)
     , "}"
     , "fun " ++ this ++ compFieldNamesCD ++ " : set FName {"
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

  Obj = #{intercalate " + " ("none" : nonAbstractClassNames)}

  // Contents
#{unlines objFNames}
  // Associations
#{unlines objAttribs}
  // Compositions
#{if anyCompositions then unlines compositions else ""}
}
|]
  where
    objFNames = map (\name -> [i|  ObjFNames[#{name}, #{name}#{fieldNamesCD}]|]) nonAbstractClassNames
    nameFromTo = \case
      Association {..} ->
        Just (associationName, associationFrom, associationTo)
      Aggregation {..} ->
        Just (aggregationName, aggregationWhole, aggregationPart)
      Composition {..} ->
        Just (compositionName, compositionWhole, compositionPart)
      Inheritance {} ->
        Nothing
    objAttribs = concatMap
      (maybe [] (uncurry3 associationFromTo) . nameFromTo)
      relationships
    associationFromTo name from to = [
      makeAssoc "Attrib" (linking from) name (linking to) (limits to),
      makeAssoc "" (linking to) name (linking from) (limits from)
      ]
    makeAssoc
      :: Show a
      => String -> String -> String -> String -> (a, Maybe a) -> String
    makeAssoc att from name to (low, Nothing) =
      [i|  ObjL#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}]|]
    makeAssoc att from name to (low, Just up) =
      [i|  ObjLU#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}, #{show up}]|]
    anyCompositions =
      any (\case Composition {} -> True; _ -> False) relationships
    compositions = map
      (\name -> [i|  Composition[#{name}#{compositesCD}, #{name}#{compFieldNamesCD}, #{name}]|])
      nonAbstractClassNames
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
