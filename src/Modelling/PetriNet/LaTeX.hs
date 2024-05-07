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
  Net (..),
  PetriMath (..),
  PetriNode (..),
  )

import Control.Arrow                ((&&&))
import Data.Char                    (isDigit)
import Data.List                    (intercalate)
import Data.Maybe                   (fromMaybe)
import Data.String.Interpolate      (i)
import Image.LaTeX.Render           (Formula)

{-|
Takes a 'Net' and generates all formulas required in order to
represent this net using a mathematical representation ('PetriMath').
-}
toPetriMath :: Net p n => p n String -> PetriMath Formula
toPetriMath pl = PetriMath {
  netMath            = netLaTeX,
  placesMath         = placesLaTeX places,
  transitionsMath    = transitionsLaTeX transitions,
  tokenChangeMath    = tokenChangeLaTeX places transitions net,
  initialMarkingMath = initialMarkingLaTeX placeNodes,
  placeOrderMath     = Just $ placeOrderLaTeX places
  }
  where
    (ps, ts)         = M.partition isPlaceNode $ nodes net
    (places, placeNodes) = unzip $ M.toList ps
    transitions      = M.keys ts
    net              = toLowerIndexes `mapNet` pl

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
netLaTeX = [i|N = #{tuple}|]
  where
    tuple :: Formula
    tuple = parenthesise
      $  placesSetName ++ ", "
      ++ transitionsSetName ++ ", \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0"

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
placeOrderLaTeX ps = [i|#{parenthesise $ intercalate "," ps}|]

{-|
Create a LaTeX-'Formula' representing the set of given places.
-}
placesLaTeX :: [String] -> Formula
placesLaTeX ps = [i|#{placesSetName} = #{brace $ intercalate "," ps}|]

{-|
Create a LaTeX-'Formula' representing the set of given transitions.
-}
transitionsLaTeX :: [String] -> Formula
transitionsLaTeX ts =
  [i|#{transitionsSetName} = #{brace $ intercalate "," ts}|]

{-|
Create a LaTeX-'Formula' representing the tuple of the inital marking.
-}
initialMarkingLaTeX
  :: PetriNode n
  => [n a]
  -- ^ A list of nodes which should only contain place nodes.
  -> Formula
initialMarkingLaTeX ns = "m_0 = "
  ++ parenthesise (intercalate "," $ show . initialTokens <$> ns)

{-|
Create LaTeX-'Formula's representing the tuples for incoming and outgoing flow
for each of the given transitions in the order of the given places.
-}
tokenChangeLaTeX
  :: Net p n
  => [String]
  -- ^ the names of the places (in this given order)
  -> [String]
  -- ^ the transition names (in this given order).
  -> p n String
  -> [(Formula, Formula)]
tokenChangeLaTeX ps ts net = (inT &&& outT) <$> ts
  where
    inT  t = "^{\\bullet}" ++ t ++ tkns (`flow` t)
    outT t = t ++ "^{\\bullet}" ++ tkns (flow t)
    tkns f  = " = " ++ parenthesise (intercalate "," $ show <$> flowList f)
    flowList f = (\p -> fromMaybe 0 $ f p net) <$> ps
