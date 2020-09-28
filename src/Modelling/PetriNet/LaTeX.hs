{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.LaTeX (
  uebung, createPetriTex, diagramTex,
  toPetriMath,
  ) where

import Modelling.PetriNet.Types

import Data.List                    (intercalate)
import Data.Maybe                   (fromMaybe)
import Data.Text                    (unpack)
import Image.LaTeX.Render           (Formula)
import Text.LaTeX
import Text.LaTeX.Base.Syntax       (getBody)
import Text.LaTeX.Packages.Inputenc 
import Text.LaTeX.Packages.Babel    (uselanguage, Language (English))
import Text.LaTeX.Packages.Graphicx

uebung :: Petri -> Int -> Bool -> LaTeX
uebung petri task switch =
    documentclass [] article
 <> usepackage [utf8] inputenc
 <> uselanguage English
 <> title "Uebung"
 <> author "Autor"
 <> document (maketitle <> body petri task switch)

body :: Petri -> Int -> Bool -> LaTeX
body petri task switch 
 | task == 1 && switch     =
   "Which of the presented petrinets shows the mathematical expression?" 
   <> newline
   <> "Given the following: "
   <> createPetriTex petri
 | task == 1 && not switch = 
   "Which of the presented mathematical expressions shows the given petrinet?" 
 | task == 2 && switch     =
   "Which pair of transitions are in conflict under the initial marking?"
 | task == 2 && not switch = 
   "Which of the following Petrinets doesn't have a conflict?"
 | task == 3 && switch     =
   "Which pair of transitions are in concurrency under the initial marking?"
 | task == 3 && not switch = 
   "Which of the following Petrinet doesn't have a concurrency?"
 | otherwise = mempty
 
--Math Petri Appearance

toPetriMath :: Petri -> PetriMath Formula
toPetriMath Petri { initialMarking, trans } = PetriMath {
  netMath            = netLaTeX,
  placesMath         = placesLaTeX $ length initialMarking,
  transitionsMath    = transitionsLaTeX $ length trans,
  tokenChangeMath    = tokenChangeLaTeX trans,
  initialMarkingMath = initialMarkingLaTeX initialMarking
  }

netLaTeX :: String
netLaTeX = mathMode $
  "N = " ++ parenthesise "P, T, ^{\\bullet}(), ()^{\\bullet}, m_0"

mathMode :: String -> String
mathMode = wrap "$" "$"

parenthesise :: String -> String
parenthesise = wrap "\\left(" "\\right)"

brace :: String -> String
brace = wrap "\\left\\{" "\\right\\}"

wrap :: [a] -> [a] -> [a] -> [a]
wrap x y zs = x ++ zs ++ y

placesLaTeX :: Int -> String
placesLaTeX n = mathMode $ "P = "
  ++ brace (intercalate "," [ "p_" ++ show x | x <- [1 .. n]])

transitionsLaTeX :: Int -> String
transitionsLaTeX n = mathMode $ "T = "
  ++ brace (intercalate "," [ "t_" ++ show x | x <- [1 .. n]])

initialMarkingLaTeX :: Marking -> String
initialMarkingLaTeX m = mathMode $ "m_0 = "
  ++ parenthesise (intercalate "," (show <$> m))

tokenChangeLaTeX :: [Transition] -> [(String, String)]
tokenChangeLaTeX ts =
  (\x -> (uncurry inT x, uncurry outT x)) <$> zip [1 :: Int ..] ts
  where
    inT  n (y, _) = mathMode $ "^{\\bullet}t" ++ tkns n y
    outT n (_, z) = mathMode $ "t^{\\bullet}" ++ tkns n z
    tkns n xs = "_" ++ show n ++ " = "
      ++ parenthesise (intercalate "," $ show <$> xs)

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

diagramTex :: Show a => [a] -> Int -> LaTeX -> LaTeX
diagramTex list i tx =
    documentclass [] article
 <> usepackage [utf8] inputenc
 <> usepackage [] graphicx
 <> uselanguage English
 <> title "Uebung"
 <> author "Autor"
 <> document (maketitle <> takeBody tx <> splitList list <> newline <> getDiagrams i)
 
takeBody :: LaTeX -> LaTeX
takeBody tx = fromMaybe (error "no default body available") (getBody tx)
  
getDiagrams :: Int -> LaTeX
getDiagrams 0 = includegraphics [IGScale 0.5] "app/0.pdf"
getDiagrams i = includegraphics [IGScale 0.5] ("app/"++show i++".pdf") <> newline <> getDiagrams (i-1)
  
splitList :: Show a => [a] -> LaTeX
splitList []     = raw ""
splitList (h:rs) = fromString (show h) <> newline <> splitList rs
