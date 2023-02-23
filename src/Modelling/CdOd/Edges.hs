{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.Edges (
  -- * Types
  AssociationType (..),
  Connection (..),
  DiagramEdge,
  -- * Getter
  connectionName,
  -- * Transformation
  fromEdges, toEdges,
  relationshipToEdge,
  renameAssocsInEdge,
  renameClassesInEdge,
  renameClasses, renameEdges,
  renameConnection,
  -- * Checks
  -- ** Check sets (reusing single checks)
  checkMultiEdge, checkObvious,
  -- ** Single checks
  calculateThickRelationships,
  compositionCycles,
  doubleConnections,
  hasAssociationAtOneSuperclass,
  inheritanceCycles,
  multipleInheritances,
  selfEdges, wrongLimits,
  anyThickEdge, shouldBeThick,
  isInheritanceEdge,
  ) where

import qualified Data.Bimap                       as BM (lookup)

import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Relationship (..),
  )
import Modelling.CdOd.Auxiliary.Util    (filterFirst)

import Control.Monad.Catch              (MonadThrow)
import Data.Bifunctor                   (first)
import Data.Bimap                       (Bimap)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (both, dupe, thd3)
import GHC.Generics                     (Generic)

data Connection
  = Inheritance'
  | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Read, Show)

data AssociationType = Association' | Aggregation' | Composition'
  deriving (Eq, Generic, Read, Show)

type DiagramEdge = (String, String, Connection)

connectionName :: Connection -> Maybe String
connectionName (Assoc _ n _ _ _) = Just n
connectionName Inheritance'      = Nothing

renameConnection
  :: MonadThrow m
  => Bimap String String
  -> Connection
  -> m Connection
renameConnection bm (Assoc t n m1 m2 b) = do
  n' <- BM.lookup n bm
  return $ Assoc t n' m1 m2 b
renameConnection _ Inheritance' = return Inheritance'

renameAssocsInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameAssocsInEdge m (f, t, a) = (f, t,) <$> renameConnection m a

renameClassesInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameClassesInEdge m (f, t, a) = (,,a) <$> rename f <*> rename t
  where
    rename = (`BM.lookup` m)

toEdges :: Cd -> [DiagramEdge]
toEdges = map relationshipToEdge . relationships

relationshipToEdge
  :: Relationship nodeName String
  -> (nodeName, nodeName, Connection)
relationshipToEdge r = case r of
  Association {..} -> (
    linking associationFrom,
    linking associationTo,
    Assoc
      Association'
      associationName
      (limits associationFrom)
      (limits associationTo)
      False
    )
  Aggregation {..} -> (
    linking aggregationWhole,
    linking aggregationPart,
    Assoc
      Aggregation'
      aggregationName
      (limits aggregationWhole)
      (limits aggregationPart)
      False
    )
  Composition {..} -> (
    linking compositionWhole,
    linking compositionPart,
    Assoc
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
  Assoc t _ _ _ True -> error
    $ "This never happens: Got a thick edge of type "
    ++ show t
  Assoc t n s e False -> case t of
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

isInheritanceEdge :: DiagramEdge -> Bool
isInheritanceEdge (_, _, Inheritance') = True
isInheritanceEdge (_, _, _          ) = False

fromEdges :: [String] -> [DiagramEdge] -> Cd
fromEdges classNames es = ClassDiagram {..}
  where
    relationships = map edgeToRelationship es

renameEdges :: Bimap String String -> [DiagramEdge] -> [DiagramEdge]
renameEdges bm es =
  [ (from, to, e')
  | (from, to, e) <- es, e' <- renameConnection bm e]

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
  [(x, y) | x@(s1, e1, Inheritance') <- es, s1 /= e1
          , y@(s2, e2, Inheritance') <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 /= e2]

inheritanceCycles :: [DiagramEdge] -> [[DiagramEdge]]
inheritanceCycles = cycles isInheritance
  where
    isInheritance Inheritance' = True
    isInheritance _           = False

compositionCycles :: [DiagramEdge] -> [[DiagramEdge]]
compositionCycles es = cycles isComposition (flatten es)
  ++ [c:cs |   (s,  e, cs) <- getPaths isInheritance es
           , c@(s', e', _) <- filter (isComposition . thd3) es
           , s' == e && s == e' || s' == s && e' == e]
  where
    isInheritance Inheritance' = True
    isInheritance _           = False

flatten :: [DiagramEdge] -> [DiagramEdge]
flatten es
  | Just ((s, e), es') <- findInheritance es
  = flatten $ flattenInheritance s e `concatMap` es'
  | otherwise
  = es

findInheritance :: [DiagramEdge] -> Maybe ((String, String), [DiagramEdge])
findInheritance []                         = Nothing
findInheritance ((s, e, Inheritance') : es) = Just ((s, e), es)
findInheritance (e:es)                     = fmap (e:) <$> findInheritance es

flattenInheritance :: String -> String -> DiagramEdge -> [DiagramEdge]
flattenInheritance s e edge@(s', e', t) = case t of
  Inheritance' | e == s', s /= e' -> [(s, e', Inheritance')]
               | s == e', e /= s' -> [(s', e, Inheritance')]
  Assoc {} | e == s' -> [(s, e', t), edge]
  Assoc {} | e == e' -> [(s', e, t), edge]
  _ -> [edge]

isComposition :: Connection -> Bool
isComposition (Assoc Composition' _ _ _ _) = True
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

anyThickEdge :: Cd -> Bool
anyThickEdge = any fst . calculateThickRelationships

calculateThickRelationships :: Cd -> [(Bool, Relationship String String)]
calculateThickRelationships ClassDiagram {..} =
  let
    classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
      where
        subs seen name
          | name `elem` seen = []
          | otherwise = name : concatMap
              (subs (name:seen) . subClass)
              (filter ((name ==) . superClass) inheritances)
    inheritances = filter
      (\case Inheritance {} -> True; _ -> False)
      relationships
    assocsBothWays = concatMap
      (map (both linking) . assocBothWays)
      relationships
    isAssocThick r = case r of
      Inheritance {} -> False
      Association {..} -> shouldBeThick
        (linking associationFrom)
        (linking associationTo)
        classesWithSubclasses
        assocsBothWays
      Aggregation {..} -> shouldBeThick
        (linking aggregationWhole)
        (linking aggregationPart)
        classesWithSubclasses
        assocsBothWays
      Composition {..} -> shouldBeThick
        (linking compositionWhole)
        (linking compositionPart)
        classesWithSubclasses
        assocsBothWays
  in map (first isAssocThick . dupe) relationships
  where
    assocBothWays Inheritance {} = []
    assocBothWays Association {..} =
      [(associationFrom, associationTo), (associationTo, associationFrom)]
    assocBothWays Aggregation {..} =
      [(aggregationPart, aggregationWhole), (aggregationWhole, aggregationPart)]
    assocBothWays Composition {..} =
      [(compositionPart, compositionWhole), (compositionWhole, compositionPart)]

shouldBeThick
  :: String
  -> String
  -> [(String, [String])]
  -> [(String, String)]
  -> Bool
shouldBeThick a b classesWithSubclasses =
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
    isInheritedUsing x (_, e, Inheritance') = x == e
    isInheritedUsing _ _                   = False
    hasAssociation _ (_, _, Inheritance') = False
    hasAssociation x (s, e, _)           = x == s || x == e
