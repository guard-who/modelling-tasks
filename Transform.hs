{-# LANGUAGE TemplateHaskell #-}

module Transform (transform) where

import Util
import Types

import Data.List
import Data.Maybe
import Data.FileEmbed

transform :: String -> Bool -> String -> ([(String, Maybe String)], [Association]) -> String
transform time template index (classes, associations) = unlines $
  concat
  [
    [ "// Alloy Model for CD" ++ index
    , "// Produced by Haskell simulation of Eclipse plugin"
    , "// Generated: " ++ time
    , ""
    , "module umlp2alloy/CD" ++ index ++ "Module"
    , ""
    , $(embedStringFile "Template.txt")
    ]
  | template ]
  ++
  [ "// Concrete names of fields"
  , associationSigs associations -- Figure 2.1, Rule 3, part 2
  , "// Classes"
  , classSigs classNames -- Figure 2.1, Rule 1, part 1
  , "///////////////////////////////////////////////////"
  , "// CD" ++ index
  , "///////////////////////////////////////////////////"
  , ""
  , "// Types wrapping subtypes"
  , subTypes index classesWithSubclasses -- Figure 2.1, Rule 1, part 2
  , "// Types wrapping composite structures and field names"
  , compositesAndFieldNames index compositions classes -- Figure 2.1, Rule 6, corrected
  , "// Relations"
  , predicate index classesWithSubclasses associations
  ]
  ++
  if template then
    [ "///////////////////////////////////////////////////"
    , "// Run commands"
    , "///////////////////////////////////////////////////"
    , ""
    , "run cd" ++ index ++ " for 5"
    ]
  else []
  where
    classNames = map fst classes
    classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
    subs seen name
      | elem name seen = []
      | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((== Just name) . snd) classes)
    compositions = filter (\(a,_,_,_,_,_) -> a == Composition) associations

associationSigs :: [Association] -> String
associationSigs = concatMap (\(_,name,_,_,_,_) -> "one sig " ++ firstLower name ++ " extends FName {}\n")

classSigs :: [String] -> String
classSigs = concatMap (\name -> "sig " ++ name ++ " extends Obj {}\n")

subTypes :: String -> [(String, [String])] -> String
subTypes index = concatMap (\(name, subclasses) -> "fun " ++ name ++ subsCD ++ ": set Obj {\n  " ++ intercalate " + " subclasses ++ "\n}\n")
  where
    subsCD = "SubsCD" ++ index

compositesAndFieldNames :: String -> [Association] -> [(String, Maybe String)] -> String
compositesAndFieldNames index compositions = unlines . concatMap (\(this, super) ->
  let thisCompositions = filter (\(_,_,_,_,to,_) -> to == this) compositions
  in
    [ "fun " ++ this ++ compositesCD ++ ": set Obj {"
    , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\(_,_,_,from,_,_) -> from ++ subsCD) thisCompositions)
    , "}"
    , "fun " ++ this ++ compFieldNamesCD ++ ": set FName {"
    , "  " ++ intercalate " + " (maybe "none" (++ compFieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> firstLower name) thisCompositions)
    , "}"
    ])
  where
    compositesCD = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD = "SubsCD" ++ index

predicate :: String -> [(String, [String])] -> [Association] -> String
predicate index classesWithSubclasses associations = unlines
  [ "pred cd" ++ index ++ " {"
  , ""
  , "  Obj = " ++ intercalate " + " classes -- Figure 2.2, Rule 5
  , ""
  , objFNames -- Figure 2.2, Rule 2, relevant portion
  , "  // Associations"
  , objAttribs -- Figure 2.3, Rule A3
  , "  // Compositions"
  , compositions -- Figure 2.2, Rule 4, corrected
  , "}"
  ]
  where classes = map fst classesWithSubclasses
        objFNames = concatMap (\name -> "  // Content of class " ++ name ++ "\n  ObjFNames[" ++ name ++ ", " ++ intercalate " + " (concatMap (\from -> map (\(_,assoc,_,_,_,_) -> firstLower assoc) (filter (\(_,_,_,this,_,_) -> from == this) associations)) (filter ((name `elem`) . fromJust . flip lookup classesWithSubclasses) classes) ++ ["none"]) ++ "]\n") classes
        objAttribs = concatMap (\(_, name, mult1, class1, class2, mult2) -> makeAssoc "Attrib" class1 name class2 mult2 ++ makeAssoc "" class2 name class1 mult1) associations
        makeAssoc att from name to (low, Nothing) = "  ObjL" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ "]\n"
        makeAssoc att from name to (low, Just up) = "  ObjLU" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ ", " ++ show up ++ "]\n"
        compositions = concatMap (\name -> "  Composition[" ++ name ++ compositesCD ++ ", " ++ name ++ compFieldNamesCD ++ ", " ++ name ++ "]\n") classes
        compositesCD = "CompositesCD" ++ index
        compFieldNamesCD = "CompFieldNamesCD" ++ index
        subsCD = "SubsCD" ++ index
