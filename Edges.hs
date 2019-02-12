module Edges (
  -- * Types
  DiagramEdge,
  -- * Transformation
  toEdges,
  -- * Checks
  compositionCycles, doubleConnections, inheritanceCycles, multipleInheritances,
  selfEdges, wrongLimits
  ) where

import Types (AssociationType (..), Connection (..), Syntax)

type DiagramEdge = (String, String, Connection)

toEdges :: Syntax -> [DiagramEdge]
toEdges (is, as) =
  [(s, e, Inheritance) | (s, Just e) <- is]
  ++ [(s, e, Assoc t m1 m2 False) | (t, _, m1, s, e, m2) <- as]

selfEdges :: [DiagramEdge] -> [DiagramEdge]
selfEdges es = [x | x@(s, e, _) <- es, e == s]

doubleConnections :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
doubleConnections es =
  [(x, y) | x@(s1, e1, _) <- es, s1 /= e1
          , y@(s2, e2, _) <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 == e2 || s1 == e2 && s2 == e1]

multipleInheritances :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
multipleInheritances es =
  [(x, y) | x@(s1, e1, Inheritance) <- es, s1 /= e1
          , y@(s2, e2, Inheritance) <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 /= e2]

inheritanceCycles :: [DiagramEdge] -> [[DiagramEdge]]
inheritanceCycles = cycles isInheritance
  where
    isInheritance Inheritance = True
    isInheritance _           = False

compositionCycles :: [DiagramEdge] -> [[DiagramEdge]]
compositionCycles = cycles isComposition
  where
    isComposition (Assoc Composition _ _ _) = True
    isComposition _                         = False

wrongLimits :: [DiagramEdge] -> [DiagramEdge]
wrongLimits es =
  [c | c@(_, _, Assoc t s@(sl, sh) e _) <- es
     , isComposition t && (sh /= Just 1 || sl < 0 || sl > 1)
       || not (inLimit s)
       || not (inLimit e)]
  where
    isComposition Composition = True
    isComposition _           = False
    inLimit (l, Nothing)
      | 0 <= l && l <= 2 = True
      | otherwise        = False
    inLimit (l, Just h)
      | l == 0 && l <  h && h <= 2 = True
      | l >  0 && l <= h && h <= 2 = True
      | otherwise                  = False

cycles :: (Connection -> Bool) -> [DiagramEdge] -> [[DiagramEdge]]
cycles connectionFilter es =
  [c:cs |   (s , e, cs) <- getPaths connectionFilter es
        , c@(s', e', _) <- edges, s' == e, s == e']
  where
    edges               = filter (connectionFilter . connection) es
    connection  (_,_,c) = c

getPaths :: (Connection -> Bool) -> [DiagramEdge] -> [(String, String, [DiagramEdge])]
getPaths connectionFilter es =
  [path | c@(s, e, _) <- edges, s /= e, path <- getPath s e [c] edges]
  where
    edges               = filter (connectionFilter . connection) es
    start       (s,_,_) = s
    end         (_,e,_) = e
    connection  (_,_,c) = c
    getPath :: String
            -> String
            -> [DiagramEdge]  -- edges within the current path
            -> [DiagramEdge]  -- still available edges
            -> [(String, String, [DiagramEdge])]
    getPath s e ps es' =
      (s, e, ps) :
      -- prevent cyclic paths
      let es'' = filter ((s /=) . start) $ filter ((s /=) . end) es'
      in [path | p@(s', e', _) <- es', s' /= e', s' /= e, s == e'
               , path <- getPath s' e (p:ps) es'']

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys
