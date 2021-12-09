{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Modelling.CdOd.Types (
  Association,
  AssociationType (..),
  Change (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  Letters (..),
  Name (..),
  NameMapping (..),
  Od,
  RelationshipProperties (..),
  Syntax,
  addedAssociation,
  associationNames,
  classNames,
  classNamesOd,
  defaultProperties,
  fromNameMapping,
  linkNames,
  parseLettersPrec,
  parseNamePrec,
  renameAssocsInCd,
  renameAssocsInEdge,
  renameClassesInCd,
  renameClassesInEdge,
  renameClassesInOd,
  renameLinksInOd,
  showLetters,
  showName,
  toNameMapping,
  toOldSyntax,
  ) where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Common       (skipSpaces)

import Control.Monad.Catch              (MonadThrow)
import Data.Bifunctor                   (first, second)
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (bimapM)
import Data.Char                        (isAlpha, isAlphaNum)
import Data.List                        (intercalate, nub)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (listToMaybe)
import Data.String                      (IsString (fromString))
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  many1,
  satisfy,
  sepBy,
  )

type Od = ([String], [(Int, Int, String)])

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving (Eq, Generic, Read, Show)

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Read, Show)

type Syntax = ([(String, [String])], [Association])

type DiagramEdge = (String, String, Connection)

newtype Name = Name { unName :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Name where
  fromString = Name

showName :: Name -> String
showName = unName

parseNamePrec :: Int -> Parser Name
parseNamePrec _ = do
  skipSpaces
  Name <$> many1 (satisfy isAlphaNum)

newtype Letters = Letters { lettersList :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Letters where
  fromString = Letters

showLetters :: Letters -> String
showLetters = lettersList

parseLettersPrec :: Int -> Parser Letters
parseLettersPrec _ = do
  skipSpaces
  Letters <$> sepBy (satisfy isAlpha) skipSpaces

newtype NameMapping = NameMapping { nameMapping :: Bimap Name Name }
  deriving (Eq, Generic)

fromNameMapping :: NameMapping -> Bimap String String
fromNameMapping = BM.mapMonotonic unName . BM.mapMonotonicR unName . nameMapping

toNameMapping :: Bimap String String -> NameMapping
toNameMapping = NameMapping . BM.mapMonotonic Name . BM.mapMonotonicR Name

instance Show NameMapping where
  show = show . BM.toList . nameMapping

instance Read NameMapping where
  readsPrec p xs = [(NameMapping $ BM.fromList y, ys) | (y, ys) <- readsPrec p xs]

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving (Foldable, Functor, Generic, Read, Show, Traversable)

data ClassConfig = ClassConfig {
    classes      :: (Int, Int),
    aggregations :: (Int, Maybe Int),
    associations :: (Int, Maybe Int),
    compositions :: (Int, Maybe Int),
    inheritances :: (Int, Maybe Int)
  } deriving (Eq, Generic, Read, Show)

data RelationshipProperties = RelationshipProperties {
    wrongAssocs             :: Int,
    wrongCompositions       :: Int,
    selfRelationships       :: Int,
    selfInheritances        :: Int,
    hasDoubleRelationships  :: Bool,
    hasReverseRelationships :: Bool,
    hasReverseInheritances  :: Bool,
    hasMultipleInheritances :: Bool,
    hasNonTrivialInheritanceCycles :: Bool,
    hasCompositionCycles    :: Bool,
    hasMarkedEdges          :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultProperties :: RelationshipProperties
defaultProperties = RelationshipProperties {
    wrongAssocs             = 0,
    wrongCompositions       = 0,
    selfRelationships       = 0,
    selfInheritances        = 0,
    hasDoubleRelationships  = False,
    hasReverseRelationships = False,
    hasReverseInheritances  = False,
    hasMultipleInheritances = False,
    hasNonTrivialInheritanceCycles = False,
    hasCompositionCycles    = False,
    hasMarkedEdges          = Nothing
  }

toOldSyntax :: Syntax -> ([(String, Maybe String)], [Association])
toOldSyntax = first (map $ second listToMaybe)

classNames :: Syntax -> [String]
classNames = map fst . fst

associationNames :: Syntax -> [String]
associationNames = map assocName . snd
  where
    assocName (_, x, _, _, _, _) = x

classNamesOd :: Od -> [String]
classNamesOd o = head . splitOn "$" <$> fst o

linkNames :: Od -> [String]
linkNames o = nub $ (\(_,_,x) -> x) `map` snd o

addedAssociation :: Change DiagramEdge -> Maybe String
addedAssociation c = add c >>= connectionName
  where
    connectionName (_, _, Assoc _ n _ _ _) = Just n
    connectionName (_, _, Inheritance)     = Nothing

renameAssocsInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameAssocsInEdge m (f, t, a) = (f, t,) <$> renameConnection a
  where
    renameConnection Inheritance           = return Inheritance
    renameConnection (Assoc ct n lf lt im) = (\n' -> Assoc ct n' lf lt im)
      <$> BM.lookup n m

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

renameClassesInEdge
  :: MonadThrow m
  => Bimap String String
  -> DiagramEdge
  -> m DiagramEdge
renameClassesInEdge m (f, t, a) = (,,a) <$> rename f <*> rename t
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

{-|
Renames all the class names by replacing class names by their new version of
the given mapping.

Object diagrams contain class names within their object names.
The class names, start the object name, they are followed by a @$@ sign.
Therefore renaming those is sufficient when renaming the classes in ODs.
There are no empty object diagram names.
(That is why the non-exhaustive pattern match is safe here.)
-}
renameClassesInOd :: MonadThrow m => Bimap String String -> Od -> m Od
renameClassesInOd m od = (,snd od) <$> mapM (rename . splitOn "$") (fst od)
  where
    rename (l:ls) = (++ '$' : intercalate "$" ls) <$> BM.lookup l m
