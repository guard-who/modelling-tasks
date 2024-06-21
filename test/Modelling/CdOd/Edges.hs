{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Modelling.CdOd.Edges (
  -- * Types
  DiagramEdge,
  -- * Transformation
  fromEdges, toEdges,
  -- * Checks
  -- ** Single checks
  compositionCycles,
  doubleConnections,
  hasAssociationAtOneSuperclass,
  inheritanceCycles,
  multipleInheritances,
  selfEdges, wrongLimits,
  ) where

import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Relationship (..),
  )
import Modelling.CdOd.Auxiliary.Util    (filterFirst)

import Data.List                        ((\\), delete)
import Data.Tuple.Extra                 (thd3)
import GHC.Generics                     (Generic)

data Connection
  = Inheritance'
  | NonInheritance AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Read, Show)

data AssociationType = Association' | Aggregation' | Composition'
  deriving (Eq, Generic, Read, Show)

type DiagramEdge = (String, String, Connection)

toEdges :: Cd -> [DiagramEdge]
toEdges = map relationshipToEdge . relationships

relationshipToEdge
  :: Relationship nodeName String
  -> (nodeName, nodeName, Connection)
relationshipToEdge r = case r of
  Association {..} -> (
    linking associationFrom,
    linking associationTo,
    NonInheritance
      Association'
      associationName
      (limits associationFrom)
      (limits associationTo)
      False
    )
  Aggregation {..} -> (
    linking aggregationWhole,
    linking aggregationPart,
    NonInheritance
      Aggregation'
      aggregationName
      (limits aggregationWhole)
      (limits aggregationPart)
      False
    )
  Composition {..} -> (
    linking compositionWhole,
    linking compositionPart,
    NonInheritance
      Composition'
      compositionName
      (limits compositionWhole)
      (limits compositionPart)
      False
    )
  Inheritance {..} -> (
    subClass,
    superClass,
    Inheritance'
    )

edgeToRelationship
  :: (nodeName, nodeName, Connection)
  -> Relationship nodeName String
edgeToRelationship (from, to, connection) = case connection of
  Inheritance' -> Inheritance {
    subClass                  = from,
    superClass                = to
    }
  NonInheritance t _ _ _ True -> error
    $ "This never happens: Got a thick edge of type "
    ++ show t
  NonInheritance t n s e False -> case t of
    Association' -> Association {
      associationName         = n,
      associationFrom         = LimitedLinking from s,
      associationTo           = LimitedLinking to e
      }
    Aggregation' -> Aggregation {
      aggregationName         = n,
      aggregationPart         = LimitedLinking to e,
      aggregationWhole        = LimitedLinking from s
      }
    Composition' -> Composition {
      compositionName         = n,
      compositionPart         = LimitedLinking to e,
      compositionWhole        = LimitedLinking from s
      }

fromEdges :: [String] -> [DiagramEdge] -> Cd
fromEdges classNames es = ClassDiagram {..}
  where
    relationships = map edgeToRelationship es

selfEdges :: [DiagramEdge] -> [DiagramEdge]
selfEdges es = [x | x@(s, e, _) <- es, e == s]

doubleConnections :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
doubleConnections es =
  [(x, y) | x@(s1, e1, _) <- es, s1 /= e1
          , y@(s2, e2, _) <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 == e2 || s1 == e2 && s2 == e1]

multipleInheritances :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
multipleInheritances es =
  [(x, y) | x@(s1, e1, Inheritance') <- es, s1 /= e1
          , y@(s2, e2, Inheritance') <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 /= e2]

inheritanceCycles :: [DiagramEdge] -> [[DiagramEdge]]
inheritanceCycles = cycles isInheritance
  where
    isInheritance Inheritance' = True
    isInheritance _           = False

compositionCycles :: [DiagramEdge] -> [[DiagramEdge]]
compositionCycles = concatMap untilCycle . segmentsOf
  where
    untilCycle (s, e, cs, remaining)
      | s == e = [cs]
      | otherwise
      = concatMap
        untilCycle
        [ (s, e', cs ++ cs', remaining')
        | (s', e', cs', remaining') <- segmentsOf remaining
        , e == s'
        ]
    segmentsOf edges =
      let compositions = filter (isComposition . thd3) edges
      in [ (s, e, [c], delete c edges)
         | c@(s, e, _) <- compositions]
         ++ [ (if s' == e then s else e, e', c:cs, edges \\ (c:cs))
            | c@(s', e', _) <- compositions
            , (s, e, cs) <- getPaths isInheritance edges
            , s' == e || s' == s]
    isInheritance Inheritance' = True
    isInheritance _           = False

isComposition :: Connection -> Bool
isComposition (NonInheritance Composition' _ _ _ _) = True
isComposition _                           = False

wrongLimits :: [DiagramEdge] -> [DiagramEdge]
wrongLimits es =
  [c | c@(_, _, t@(NonInheritance _ _ s@(sl, sh) e _)) <- es
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

hasAssociationAtOneSuperclass :: [String] -> [DiagramEdge] -> Bool
hasAssociationAtOneSuperclass cs es = any inheritanceHasOtherEdge cs
  where
    inheritanceHasOtherEdge x = any (x `isInheritedUsing`) es
      && any (x `hasAssociation`) es
    isInheritedUsing x (_, e, Inheritance') = x == e
    isInheritedUsing _ _                   = False
    hasAssociation _ (_, _, Inheritance') = False
    hasAssociation x (s, e, _)           = x == s || x == e
