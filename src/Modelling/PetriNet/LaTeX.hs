{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.LaTeX (uebung,createPetriTex) where

import Modelling.PetriNet.Types

import Data.Text                    (unpack)
import Text.LaTeX
import Text.LaTeX.Packages.Inputenc 
import Text.LaTeX.Packages.Babel    (uselanguage, Language (English))
import Text.LaTeX.Packages.Geometry

--PetriNet -> Choose task 
-- runTex :: Petri -> Int -> IO()
-- runTex petri t = do
  -- renderFile ("task"++show t++".tex") $ uebung petri t
  -- print $ "Task"++show t++" generated"

uebung :: Petri -> Int -> Bool -> LaTeX
uebung petri task switch =
    documentclass [] article
 <> usepackage [utf8] inputenc
 <> uselanguage English
 <> importGeometry [GCentered]
 <> title "Uebung"
 <> author "Autor"
 <> document (maketitle <> body petri task switch)

body :: Petri -> Int -> Bool -> LaTeX
body petri task switch 
 | task == 1 && switch     = task1 <> createPetriTex petri
 | task == 1 && not switch = task1a
 | task == 2 && switch     = task2
 | task == 2 && not switch = task2a
 | otherwise = mempty
 
--Math Petri Appearance
createPetriTex :: Petri -> LaTeX
createPetriTex Petri{initialMarking,trans} =
  math ( "N = (P, T," <> raw "^{\\bullet}" <> "(), ()" 
  <> raw "^{\\bullet}" <> ", m" <> raw "_" <> "0)")
  <> math ( raw "P = \\{" <> createPlaces 1 (length initialMarking) <> raw "\\}" )
  <> " , "
  <> math ( raw "T = \\{" <> createTrans 1 (length trans) <> raw "\\}" )
  <> " , "
  <> math ( raw "m_0 = (" <> fromString (unpack (renderCommas initialMarking)) <> ")")
  <> itemize ( conditions 1 trans )

--MatchToMath
task1 :: LaTeX
task1 = 
  "Which of the presented petrinets shows the mathematical expression?" 
  <> newline
  <> "Given the following: "
  
task1a :: LaTeX
task1a = 
  "Which of the presented mathematical expressions shows the given petrinet?" 
  
--FindConflicts
task2 :: LaTeX
task2 = 
  "Which pair of transitions are in conflict under the initial marking in each Petrinet?"
  
task2a :: LaTeX
task2a =
  "Which of the following Petrinets doesn't have a conflict?"

createPlaces ::Int -> Int -> LaTeX
createPlaces i p 
 | i < p     = raw "p_" <> fromString (show i :: String) <> "," 
                 <> createPlaces (i+1) p
 | i == p    = raw "p_" <> fromString (show i :: String)
 | otherwise = mempty

createTrans :: Int -> Int -> LaTeX
createTrans i t 
 | i < t     = raw "t_" <> fromString (show i :: String) <> ","
                 <> createTrans (i+1) t
 | i == t    = raw "t_" <> fromString (show i :: String)
 | otherwise = mempty

conditions :: Int -> [Transition] -> LaTeX
conditions _ []          = mempty
conditions i ((pr,po):rs)=
  item Nothing <> math (raw "^{\\bullet}" <> fromString("t" ::String) 
    <> raw "_" <> fromString(show i :: String) 
    <> fromString ("= ("++ unpack (  renderCommas pr) ++ ")" :: String))
  <> item Nothing <> math ( fromString ( "t" :: String) 
    <> raw "_" <> fromString (show i :: String) <> raw "^{\\bullet}"
    <> fromString (" = ("++ unpack (  renderCommas po) ++ ")" :: String))
  <> conditions (i+1) rs
