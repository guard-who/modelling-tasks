{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Modelling.CdOd.CD2Alloy.Transform (
  createRunCommand, mergeParts, transform,
  ) where

import Modelling.CdOd.Types             (Association, AssociationType(..))

import Data.List
import Data.FileEmbed
import Data.Maybe                       (catMaybes, isJust)
import Data.String.Interpolate          (i)

transform
  :: ([(String, Maybe String)], [Association])
  -> Maybe Bool
  -> Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> String
  -> String
  -> (String, String, String, String, String)
transform
  (classes, associations)
  hasSelfLoops
  noIsolationLimitation
  minLinks
  maxLinks
  minLinksPerObject
  maxLinksPerObject
  index
  time =
  (part1, part2, part3, part4, part5)
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
#{limitLinks}
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
    limitLinks = withJusts (\ps -> [i|
fact LimitLinks {
#{unlines ps}
}
|]) [
      ("  #get >= " ++) . show <$> minLinks,
      ("  #get <= " ++) . show <$> maxLinks,
      linksPerObject minLinksPerObject maxLinksPerObject]
    linksPerObject Nothing Nothing = Nothing
    linksPerObject mmin mmax = Just $
      "  all o : Obj | let x = minus[plus[#o.get,#get.o],#o.get.o] |"
      ++ foldMap ((" x >= " ++) . show) mmin
      ++ foldMap (const " &&") (mmin >> mmax)
      ++ foldMap ((" x <= " ++) . show) mmax
    part2 = [i|
// Concrete names of fields
#{unlines (associationSigs associations)}
|]
    part3 = [i|
// Classes
#{unlines (classSigs classNames)}
|]
    part4 = [i|
///////////////////////////////////////////////////
// CD#{index}
///////////////////////////////////////////////////

// Types wrapping subtypes
#{unlines (subTypes index classesWithDirectSubclasses)}
// Types wrapping field names
#{unlines (fieldNames index associations classes)}
// Types wrapping composite structures and field names
#{unlines (compositesAndFieldNames index compositions classes)}
// Properties
#{predicate index associations classNames}
|]
    part5 = createRunCommand hasSelfLoops ("cd" ++ index) (length classes) 5
    classNames = map fst classes
    classesWithDirectSubclasses =
      map (\(name, _) -> (name, map fst (filter ((== Just name) . snd) classes))) classes
    compositions = filter (\(a,_,_,_,_,_) -> a == Composition) associations

createRunCommand :: Maybe Bool -> String -> Int -> Int -> String
createRunCommand hasSelfLoops command numClasses maxObjects = [i|
///////////////////////////////////////////////////
// Run commands
///////////////////////////////////////////////////
fact {
  #{loops}
}

run { #{command} } for #{maxObjects} Obj, #{intSize} FName, #{intSize} Int
|]
  where
    intSize :: Int
    intSize = ceiling intSize'
    intSize' :: Double
    intSize' = logBase 2 $ fromIntegral $
      2 * max (numClasses * maxObjects) (2 * maxObjects) + 1
    loops            = case hasSelfLoops of
      Nothing    -> ""
      Just True  -> "some o : Obj | o in o.get[FName]"
      Just False -> "no o : Obj | o in o.get[FName]"

associationSigs :: [Association] -> [String]
associationSigs = map (\(_,name,_,_,_,_) -> "one sig " ++ name ++ " extends FName {}")

classSigs :: [String] -> [String]
classSigs = map (\name -> "sig " ++ name ++ " extends Obj {}")

subTypes :: String -> [(String, [String])] -> [String]
subTypes index = concatMap (\(name, directSubclasses) ->
  [ "fun " ++ name ++ subsCD ++ " : set Obj {"
  , "  " ++ intercalate " + " (name : map (++ subsCD) directSubclasses)
  , "}"
  ])
  where
    subsCD = "SubsCD" ++ index

fieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
fieldNames index associations = concatMap (\(this, super) ->
  let thisAssociations = filter (\(_,_,_,from,_,_) -> from == this) associations
  in [ "fun " ++ this ++ fieldNamesCD ++" : set FName {"
     , "  " ++ intercalate " + " (maybe "none" (++ fieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> name) thisAssociations)
     , "}"
     ])
  where
    fieldNamesCD = "FieldNamesCD" ++ index

compositesAndFieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
compositesAndFieldNames index compositions = concatMap (\(this, super) ->
  let thisCompositions = filter (\(_,_,_,_,to,_) -> to == this) compositions
  in [ "fun " ++ this ++ compositesCD ++ " : set Obj {"
     , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\(_,_,_,from,_,_) -> from ++ subsCD) thisCompositions)
     , "}"
     , "fun " ++ this ++ compFieldNamesCD ++ " : set FName {"
     , "  " ++ intercalate " + " (maybe "none" (++ compFieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> name) thisCompositions)
     , "}"
     ])
  where
    compositesCD = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD = "SubsCD" ++ index

predicate :: String -> [Association] -> [String] -> String
predicate index associations classNames = [i|
pred cd#{index} {

  Obj = #{intercalate " + " classNames}

  // Contents
#{unlines objFNames}
  // Associations
#{unlines objAttribs}
  // Compositions
#{unlines compositions}
}
|]
  where
    objFNames = map (\name -> [i|  ObjFNames[#{name}, #{name}#{fieldNamesCD}]|]) classNames
    objAttribs = concatMap
      (\(_, name, mult1, class1, class2, mult2) ->
          [makeAssoc "Attrib" class1 name class2 mult2
          , makeAssoc "" class2 name class1 mult1])
      associations
    makeAssoc
      :: Show a
      => String -> String -> String -> String -> (a, Maybe a) -> String
    makeAssoc att from name to (low, Nothing) =
      [i|  ObjL#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}]|]
    makeAssoc att from name to (low, Just up) =
      [i|  ObjLU#{att}[#{from}#{subsCD}, #{name}, #{to}#{subsCD}, #{show low}, #{show up}]|]
    compositions = map
      (\name -> [i|  Composition[#{name}#{compositesCD}, #{name}#{compFieldNamesCD}, #{name}]|])
      classNames
    fieldNamesCD     = "FieldNamesCD" ++ index
    compositesCD     = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD           = "SubsCD" ++ index

mergeParts
  :: (String, String, String, String)
  -> (String, String, String, String)
  -> (String, String, String, String)
mergeParts (p1, p2, p3, p4) (_, p2', p3', p4') =
  (p1, p2 `unionL` p2', p3 `unionL` p3', p4 ++ p4')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y
