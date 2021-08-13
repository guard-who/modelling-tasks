module Modelling.CdOd.Edges (
  -- * Types
  DiagramEdge,
  -- * Transformation
  fromEdges, toEdges,
  renameClasses, renameEdges,
  -- * Checks
  -- ** Check sets (reusing single checks)
  checkMultiEdge, checkObvious,
  -- ** Single checks
  compositionCycles,
  doubleConnections,
  hasAssociationAtOneSuperclass,
  inheritanceCycles,
  multipleInheritances,
  selfEdges, wrongLimits,
  anyMarkedEdge, shouldBeMarked
  ) where

import qualified Data.Bimap                       as BM (lookup)

import Modelling.CdOd.Types
  (AssociationType (..), Connection (..), DiagramEdge, Syntax)
import Modelling.CdOd.Auxiliary.Util    (filterFirst)

import Data.Bimap                       (Bimap)
import Data.List                        (partition)
import Data.Maybe                       (fromJust)

toEdges :: Syntax -> [DiagramEdge]
toEdges (is, as) =
  [(s, e, Inheritance) | (s, es) <- is, e <- es]
  ++ [(s, e, Assoc t n m1 m2 False) | (t, n, m1, s, e, m2) <- as]

fromEdges :: [String] -> [DiagramEdge] -> Syntax
fromEdges classNames es =
  let isInheritance (_, _, Inheritance) = True
      isInheritance (_, _, _          ) = False
      (ihs, ass) = partition isInheritance es
      classes' = map
        (\x -> (x, [e | (s, e, Inheritance) <- ihs, s == x]))
        classNames
      assocs   = [(t, n, m1, s, e, m2) | (s, e, Assoc t n m1 m2 False) <- ass]
  in (classes', assocs)

renameEdges :: Bimap String String -> [DiagramEdge] -> [DiagramEdge]
renameEdges bm es =
  [ (from, to, e')
  | (from, to, e) <- es, e' <- rename e]
  where
    rename (Assoc t n m1 m2 b) = [Assoc t n' m1 m2 b | n' <- BM.lookup n bm]
    rename e@Inheritance       = [e]

renameClasses :: Bimap String String -> [DiagramEdge] -> [DiagramEdge]
renameClasses bm es =
  [ (from', to', e)
  | (from, to, e) <- es, from' <- BM.lookup from bm, to' <- BM.lookup to bm]

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

isComposition :: Connection -> Bool
isComposition (Assoc Composition _ _ _ _) = True
isComposition _                           = False

wrongLimits :: [DiagramEdge] -> [DiagramEdge]
wrongLimits es =
  [c | c@(_, _, t@(Assoc _ _ s@(sl, sh) e _)) <- es
     , isComposition t && (sh /= Just 1 || sl < 0 || sl > 1)
       || not (inLimit s)
       || not (inLimit e)]
  where
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

anyMarkedEdge :: Syntax -> Bool
anyMarkedEdge (classes, associations) =
  let
    classesWithSubclasses = map (\(name, _) -> (name, subs [] name)) classes
      where
        subs seen name
          | name `elem` seen = []
          | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((name `elem`) . snd) classes)
    assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  in any (\(_,_,_,from,to,_) -> shouldBeMarked from to classesWithSubclasses assocsBothWays) associations

shouldBeMarked :: String -> String -> [(String, [String])] -> [(String, String)] -> Bool
shouldBeMarked a b classesWithSubclasses =
  any (\(a',b') ->
         (a /= a' || b /= b')
         && let { one = a' `isSubOf` a; two = b' `isSubOf` b }
            in (one && (two || b `isSubOf` b') || two && (one || a `isSubOf` a'))
      )
  where x `isSubOf` y = x `elem` fromJust (lookup y classesWithSubclasses)

checkMultiEdge :: [DiagramEdge] -> Bool
checkMultiEdge cs =
     null (doubleConnections cs)
  && null (multipleInheritances cs)
  && null (inheritanceCycles cs)
  && null (compositionCycles cs)

checkObvious :: [DiagramEdge] -> Bool
checkObvious cs =
     null (selfEdges cs)
  && null (wrongLimits cs)

hasAssociationAtOneSuperclass :: [String] -> [DiagramEdge] -> Bool
hasAssociationAtOneSuperclass cs es = any inheritanceHasOtherEdge cs
  where
    inheritanceHasOtherEdge x = any (x `isInheritedUsing`) es
      && any (x `hasAssociation`) es
    isInheritedUsing x (_, e, Inheritance) = x == e
    isInheritedUsing _ _                   = False
    hasAssociation _ (_, _, Inheritance) = False
    hasAssociation x (s, e, _)           = x == s || x == e
