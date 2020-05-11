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

uebung :: Petri -> Int -> LaTeX
uebung petri task =
    documentclass [] article
 <> usepackage [utf8] inputenc
 <> uselanguage English
 <> importGeometry [GCentered]
 <> title "Uebung"
 <> author "Autor"
 <> document (maketitle <> body petri task)

body :: Petri -> Int -> LaTeX
body petri task 
 | task == 1 = task1 <> createPetriTex petri
 | task == 2 = task1a
 | otherwise = mempty
 
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

task1 :: LaTeX
task1 = 
  "Which of the presented petrinets shows the mathematical expression?" 
  <> newline
  <> "Given the following: "
  
task1a :: LaTeX
task1a = 
  "Which of the presented mathematical expressions shows the given petrinet?" 

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
