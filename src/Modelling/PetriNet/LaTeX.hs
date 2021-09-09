{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
This module provides functionality of generating LaTeX formula in order to
represent a given 'PetriLike'.
Using this module these formulas are only converted into LaTeX source code.
-}
module Modelling.PetriNet.LaTeX (
  toPetriMath,
  ) where

import qualified Data.Map as M

import Modelling.PetriNet.Types (
  Node (..), PetriLike (..), PetriMath (..),
  isPlaceNode, mapPetriLike,
  )

import Control.Arrow                ((&&&))
import Data.Char                    (isDigit)
import Data.List                    (intercalate)
import Data.Maybe                   (fromMaybe)
import Data.String.Interpolate      (i)
import Image.LaTeX.Render           (Formula)

{-|
Takes a 'PetriLike' net and generates all formulas required in order to
represent this net using a mathematical representation ('PetriMath').
-}
toPetriMath :: PetriLike String -> PetriMath Formula
toPetriMath pl = PetriMath {
  netMath            = netLaTeX,
  placesMath         = placesLaTeX places,
  transitionsMath    = transitionsLaTeX $ fst <$> transitions,
  tokenChangeMath    = tokenChangeLaTeX places transitions,
  initialMarkingMath = initialMarkingLaTeX pnodes,
  placeOrderMath     = Just $ placeOrderLaTeX places
  }
  where
    (ps, ts)         = M.partition isPlaceNode allNodes
    (places, pnodes) = unzip $ M.toList ps
    transitions      = M.toList ts
    PetriLike {allNodes} = toLowerIndexes `mapPetriLike` pl

{-|
Rewrite the given 'String' to print indexes as subscripts when rendering it
using LaTeX.

>>> toLowerIndexes "t1"
"t_{1}"
-}
toLowerIndexes :: String -> Formula
toLowerIndexes [] = []
toLowerIndexes (x:xs)
  | isDigit x = "_{" ++ x : ys ++ '}' : toLowerIndexes zs
  | otherwise = x : toLowerIndexes xs
  where
    (ys, zs) = span isDigit xs

placesSetName :: String
placesSetName = "S"

transitionsSetName :: String
transitionsSetName = "T"

{-|
A LaTeX-'Formula' for the basic five tuple representing a Petri net.
-}
netLaTeX :: Formula
netLaTeX = mathMode [i|N = #{tuple}|]
  where
    tuple :: Formula
    tuple = parenthesise
      $  placesSetName ++ ", "
      ++ transitionsSetName ++ ", \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0"

{-|
Switch the mode for the given LaTeX-'Formula' to Math mode while in text mode
and vice versa otherwise.
-}
mathMode :: Formula -> Formula
mathMode = wrap "$" "$"

{-|
Wrap the given LaTeX-'Formula' into parentheses.
-}
parenthesise :: Formula -> Formula
parenthesise = wrap "\\left(" "\\right)"

{-|
Wrap the given LaTeX-'Formula' into braces.
-}
brace :: Formula -> Formula
brace = wrap "\\left\\{" "\\right\\}"

{-|
Wrap the third given 'Formula' between the first and second 'Formula'.
-}
wrap :: Formula -> Formula -> Formula -> Formula
wrap x y zs = [i|#{x}#{zs}#{y}|]

{-|
Create a LaTeX-'Formula' representing the order of places.
-}
placeOrderLaTeX :: [String] -> Formula
placeOrderLaTeX ps = mathMode [i|#{parenthesise $ intercalate "," ps}|]

{-|
Create a LaTeX-'Formula' representing the set of given places.
-}
placesLaTeX :: [String] -> Formula
placesLaTeX ps = mathMode [i|#{placesSetName} = #{brace $ intercalate "," ps}|]

{-|
Create a LaTeX-'Formula' representing the set of given transitions.
-}
transitionsLaTeX :: [String] -> Formula
transitionsLaTeX ts =
  mathMode [i|#{transitionsSetName} = #{brace $ intercalate "," ts}|]

{-|
Create a LaTeX-'Formula' representing the tuple of the inital marking.
-}
initialMarkingLaTeX
  :: [Node a]
  -- ^ A list of nodes which should only contain 'PlaceNode's.
  -> Formula
initialMarkingLaTeX ns = mathMode $ "m_0 = "
  ++ parenthesise (intercalate "," $ show . initial <$> ns)

{-|
Create LaTeX-'Formula's representing the tuples for incoming and outgoing flow
for each of the given transitions in the order of the given places.
-}
tokenChangeLaTeX
  :: [String]
  -- ^ the names of the places (in this given order)
  -> [(String, Node String)]
  -- ^ The transitions together with their names.
  --   This List should only contain 'TransitionNode's.
  -> [(Formula, Formula)]
tokenChangeLaTeX ps ts = (uncurry inT &&& uncurry outT) <$> ts
  where
    inT  n t = mathMode $ "^{\\bullet}" ++ n ++ tkns (flowIn t)
    outT n t = mathMode $ n ++ "^{\\bullet}" ++ tkns (flowOut t)
    tkns xs  = " = " ++ parenthesise (intercalate "," $ show <$> flowList xs)
    flowList xs = (\x -> fromMaybe 0 $ M.lookup x xs) <$> ps
