{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module PetriTex where

import Data.Text                    (unpack)
import Text.LaTeX
--import Text.LaTeX.Packages.AMSMath  (bullet)
import Text.LaTeX.Packages.Inputenc 
import Text.LaTeX.Packages.Babel    (uselanguage, Language (English))
import Text.LaTeX.Packages.Geometry
import Types

runTex :: Petri -> Int -> IO()
runTex petri t = renderFile "testA.tex" $ uebung petri t

uebung :: Petri -> Int -> LaTeX
uebung petri task =
    documentclass [] article
 <> usepackage [utf8] inputenc
 <> uselanguage English
 <> importGeometry [GCentered]
 <> title "Uebung"
 <> author "Autor"
 <> document (maketitle <> (body petri task))

body :: Petri -> Int -> LaTeX
body Petri{startM,trans} task 
 | task == 1 = (task1 startM (length trans))  <> itemize ( conditions 1 trans )
 | otherwise = mempty

task1 :: Mark -> Int -> LaTeX
task1 m tl = 
  "Which of the presented petrinets shows the mathematical expression?" 
  <> linebreak 0
  <> "Given the following: "
  <> math ( "N = (P, T," <> raw "^{\\bullet}" <> "(), ()" 
  <> raw "^{\\bullet}" <> ", m" <> raw "_" <> "0)")
  <> math ( raw "P = \\{" <> createPlaces 1 (length m) <> raw "\\}" )
  <> " , "
  <> math ( raw "T = \\{" <> createTrans 1 tl <> raw "\\}" )
  <> " , "
  <> math ( raw "m_0 = (" <>fromString (unpack (renderCommas m)) <> ")") 

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

conditions :: Int -> [Trans] -> LaTeX
conditions _ []          = mempty
conditions i ((pr,po):rs)=
  item Nothing <> math (raw "^{\\bullet}" <> fromString("t" ::String) 
    <> raw "_" <> fromString(show i :: String) 
    <> fromString ("= ("++ unpack (  renderCommas pr) ++ ")" :: String))
  <> item Nothing <> math ( fromString ( "t" :: String) 
    <> raw "_" <> fromString (show i :: String) <> raw "^{\\bullet}"
    <> fromString (" = ("++ unpack (  renderCommas po) ++ ")" :: String))
  <> conditions (i+1) rs