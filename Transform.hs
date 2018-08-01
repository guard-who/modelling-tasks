{-# LANGUAGE TemplateHaskell #-}

module Transform (transform) where

import Util

import Data.List
import Data.Maybe
import Data.FileEmbed

transform time template index (classes, associations) = unlines $
  concat
  [
    [ "// Alloy Model for CD" ++ index
    , "// Produced by Haskell simulation of Eclipse plugin"
    , "// Generated: " ++ show time
    , ""
    , "module umlp2alloy/CD" ++ index ++ "Module"
    , ""
    , $(embedStringFile "Template.txt")
    ]
  | template ]
  ++
  [ "// Concrete names of fields"
  , associationSigs associations
  , "// Classes"
  , classSigs classNames
  , "///////////////////////////////////////////////////"
  , "// CD" ++ index
  , "///////////////////////////////////////////////////"
  , ""
  , "// Types wrapping subtypes"
  , subTypes index classesWithSubclasses
  , "// Relations"
  , predicate index classesWithSubclasses associations
  ]
  ++
  if template then
    [ ""
    , "///////////////////////////////////////////////////"
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

associationSigs = concatMap (\(name,_,_,_,_) -> "one sig " ++ firstLower name ++ " extends FName {}\n")

classSigs = concatMap (\name -> "sig " ++ name ++ " extends Obj {}\n")

subTypes index = concatMap (\(name, subclasses) -> "fun " ++ name ++ subsCD ++ ": set Obj {\n  " ++ intercalate " + " subclasses ++ "\n}\n")
  where
    subsCD = "SubsCD" ++ index

predicate index classesWithSubclasses associations = "pred cd" ++ index ++ " {\n\n" ++ objFNames ++ "\n  // Associations\n" ++ objAttribs ++ "\n}"
  where classes = map fst classesWithSubclasses
        objFNames = concatMap (\name -> "  // Definition of class " ++ name ++ "\n  ObjFNames[" ++ name ++ ", " ++ intercalate " + " (concatMap (\from -> map (\(assoc,_,_,_,_) -> firstLower assoc) (filter (\(_,_,this,_,_) -> from == this) associations)) (filter ((name `elem`) . fromJust . flip lookup classesWithSubclasses) classes) ++ ["none"]) ++ "]\n") classes
        objAttribs = concatMap (\(name, mult1, class1, class2, mult2) -> makeAssoc "Attrib" class1 name class2 mult2 ++ makeAssoc "" class2 name class1 mult1) associations
        makeAssoc att from name to (low, Nothing) = "  ObjL" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ "]\n"
        makeAssoc att from name to (low, Just up) = "  ObjLU" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ ", " ++ show up ++ "]\n"
        subsCD = "SubsCD" ++ index
