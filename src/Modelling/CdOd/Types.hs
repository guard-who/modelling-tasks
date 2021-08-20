{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.Types (
  Association,
  AssociationType (..),
  Change (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  Od,
  RelationshipProperties (..),
  Syntax,
  defaultProperties,
  toOldSyntax,
  renameClassesInCd,
  renameClassesInOd,
  renameAssocsInCd,
  renameLinksInOd,
  ) where

import qualified Data.Bimap                       as BM

import Control.Monad.Catch              (MonadThrow)
import Data.Bifunctor                   (first, second)
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (bimapM)
import Data.Maybe                       (listToMaybe)
import GHC.Generics                     (Generic)

type Od = ([String], [(Int, Int, String)])

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving (Eq, Generic, Show)

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Show)

type Syntax = ([(String, [String])], [Association])

type DiagramEdge = (String, String, Connection)

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving (Functor, Generic, Show)

data ClassConfig = ClassConfig {
    classes      :: (Int, Int),
    aggregations :: (Int, Maybe Int),
    associations :: (Int, Maybe Int),
    compositions :: (Int, Maybe Int),
    inheritances :: (Int, Maybe Int)
  } deriving (Eq, Generic)

data RelationshipProperties = RelationshipProperties {
    wrongAssocs             :: Int,
    wrongCompositions       :: Int,
    selfRelationships       :: Int,
    hasDoubleRelationships  :: Bool,
    hasReverseRelationships :: Bool,
    hasMultipleInheritances :: Bool,
    hasInheritanceCycles    :: Bool,
    hasCompositionCycles    :: Bool,
    hasMarkedEdges          :: Maybe Bool
  } deriving Generic

defaultProperties :: RelationshipProperties
defaultProperties = RelationshipProperties {
    wrongAssocs             = 0,
    wrongCompositions       = 0,
    selfRelationships       = 0,
    hasDoubleRelationships  = False,
    hasReverseRelationships = False,
    hasMultipleInheritances = False,
    hasInheritanceCycles    = False,
    hasCompositionCycles    = False,
    hasMarkedEdges          = Nothing
  }

toOldSyntax :: Syntax -> ([(String, Maybe String)], [Association])
toOldSyntax = first (map $ second listToMaybe)

renameAssocsInCd :: MonadThrow m => Bimap String String -> Syntax -> m Syntax
renameAssocsInCd m cd = (fst cd,) <$> mapM (renameAssocsInAssociation m) (snd cd)

renameAssocsInAssociation
  :: MonadThrow m
  => Bimap String String
  -> Association
  -> m Association
renameAssocsInAssociation m (t, n, fl, fc, tc, tl) = do
  n' <- rename n
  return (t, n', fl, fc, tc, tl)
  where
    rename = (`BM.lookup` m)


renameClassesInCd :: MonadThrow m => Bimap String String -> Syntax -> m Syntax
renameClassesInCd m cd = (,)
  <$> mapM (bimapM rename $ mapM rename) (fst cd)
  <*> mapM (renameClassesInAssociation m) (snd cd)
  where
    rename = (`BM.lookup` m)

renameClassesInAssociation
  :: MonadThrow m
  => Bimap String String
  -> Association
  -> m Association
renameClassesInAssociation m (t, n, fl, fc, tc, tl) = do
  fc' <- rename fc
  tc' <- rename tc
  return (t, n, fl, fc', tc', tl)
  where
    rename = (`BM.lookup` m)

renameLinksInOd :: MonadThrow m => Bimap String String -> Od -> m Od
renameLinksInOd m od = (fst od,) <$> mapM rename (snd od)
  where
    rename (f, t, l) = (f,t,) <$> BM.lookup l m

renameClassesInOd :: MonadThrow m => Bimap String String -> Od -> m Od
renameClassesInOd m od = (,snd od) <$> mapM rename (fst od)
  where
    rename (l:ls) = (++ ls) <$> BM.lookup [l] m
    rename []     = BM.lookup [] m
