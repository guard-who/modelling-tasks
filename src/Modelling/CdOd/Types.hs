{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Modelling.CdOd.Types (
  AssociationType (..),
  Cd,
  Change (..),
  ClassConfig (..),
  ClassDiagram (..),
  Connection (..),
  DiagramEdge,
  Letters (..),
  LimitedLinking (..),
  Name (..),
  NameMapping (..),
  ObjectConfig (..),
  Od,
  Relationship (..),
  RelationshipProperties (..),
  associationNames,
  checkClassConfig,
  checkClassConfigWithProperties,
  classNamesOd,
  connectionName,
  defaultProperties,
  fromNameMapping,
  linkNames,
  maxFiveObjects,
  maxRels,
  parseLettersPrec,
  parseNamePrec,
  relationshipName,
  renameAssocsInEdge,
  renameClassesAndRelationshipsInCd,
  renameClassesInEdge,
  renameClassesInOd,
  renameConnection,
  renameLinksInOd,
  reverseAssociation,
  showLetters,
  showName,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  toNameMapping,
  ) where


import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Common       (skipSpaces)

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random             (MonadRandom)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (Bitraversable (bitraverse))
import Data.Char                        (isAlpha, isAlphaNum)
import Data.List                        (intercalate, nub)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe, mapMaybe)
import Data.String                      (IsString (fromString))
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)
import Text.ParserCombinators.Parsec (
  Parser,
  many1,
  satisfy,
  endBy,
  )

type Od = ([String], [(Int, Int, String)])

shuffleObjectAndLinkOrder :: (MonadRandom m, MonadThrow m) => Od -> m Od
shuffleObjectAndLinkOrder (objects, links) = do
  (is', objects') <- unzip <$> shuffleM (zip [0..] objects)
  let isBM = BM.fromList $ zip [0..] is'
      changeIs (x, y, z) = (,,)
        <$> BM.lookupR x isBM
        <*> BM.lookupR y isBM
        <*> pure z
  links' <- mapM changeIs links >>= shuffleM
  return (objects', links')

{-|
A meta-level connection to a node name
with a (possibly invalid) range of multiplicities
limiting the number of possible (non-meta-level) connections
using this specific connector.
-}
data LimitedLinking nodeName = LimitedLinking {
  linking                     :: nodeName,
  limits                      :: (Int, Maybe Int)
  }
  deriving (Eq, Functor, Foldable, Generic, Read, Show, Traversable)

{-|
All possible relationships within a `ClassDiagram`.
-}
data Relationship className relationshipName
  = Association {
    associationName           :: relationshipName,
    associationFrom           :: LimitedLinking className,
    associationTo             :: LimitedLinking className
    }
  | Aggregation {
    aggregationName           :: relationshipName,
    aggregationPart           :: LimitedLinking className,
    aggregationWhole          :: LimitedLinking className
    }
  | Composition {
    compositionName           :: relationshipName,
    compositionPart           :: LimitedLinking className,
    compositionWhole          :: LimitedLinking className
    }
  | Inheritance {
    subClass                  :: className,
    superClass                :: className
    }
  deriving (Eq, Generic, Read, Show)

instance Bifunctor Relationship where
  bimap f g r = case r of
    Association {..} -> Association {
      associationName         = g associationName,
      associationFrom         = fmap f associationFrom,
      associationTo           = fmap f associationTo
      }
    Aggregation {..} -> Aggregation {
      aggregationName         = g aggregationName,
      aggregationPart         = fmap f aggregationPart,
      aggregationWhole        = fmap f aggregationWhole
      }
    Composition {..} -> Composition {
      compositionName         = g compositionName,
      compositionPart         = fmap f compositionPart,
      compositionWhole        = fmap f compositionWhole
      }
    Inheritance {..} -> Inheritance {
      subClass                = f subClass,
      superClass              = f superClass
      }

instance Bifoldable Relationship where
  bifoldMap f g r = case r of
    Association {..} -> g associationName
      <> foldMap f associationFrom
      <> foldMap f associationTo
    Aggregation {..} -> g aggregationName
      <> foldMap f aggregationPart
      <> foldMap f aggregationWhole
    Composition {..} -> g compositionName
      <> foldMap f compositionPart
      <> foldMap f compositionWhole
    Inheritance {..} -> f subClass
      <> f superClass

instance Bitraversable Relationship where
  bitraverse f g r = case r of
    Association {..} -> Association
      <$> g associationName
      <*> traverse f associationFrom
      <*> traverse f associationTo
    Aggregation {..} -> Aggregation
      <$> g aggregationName
      <*> traverse f aggregationPart
      <*> traverse f aggregationWhole
    Composition {..} -> Composition
      <$> g compositionName
      <*> traverse f compositionPart
      <*> traverse f compositionWhole
    Inheritance {..} -> Inheritance
      <$> f subClass
      <*> f superClass

relationshipName :: Relationship c r -> Maybe r
relationshipName x = case x of
  Association {..} -> Just associationName
  Aggregation {..} -> Just aggregationName
  Composition {..} -> Just compositionName
  Inheritance {}   -> Nothing

reverseAssociation :: Relationship c r -> Relationship c r
reverseAssociation x = case x of
  Association {..} -> Association {
    associationName           = associationName,
    associationFrom           = associationTo,
    associationTo             = associationFrom
    }
  Aggregation {} -> x
  Composition {} -> x
  Inheritance {} -> x

data AssociationType = Association' | Aggregation' | Composition'
  deriving (Eq, Generic, Read, Show)

data Connection = Inheritance' | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving (Eq, Generic, Read, Show)

data ClassDiagram className relationshipName = ClassDiagram {
  classNames                  :: [className],
  relationships               :: [Relationship className relationshipName]
  }
  deriving (Eq, Generic, Read, Show)

instance Bifunctor ClassDiagram where
  bimap f g ClassDiagram {..} = ClassDiagram {
    classNames  = map f classNames,
    relationships = map (bimap f g) relationships
    }

instance Bifoldable ClassDiagram where
  bifoldMap f g ClassDiagram {..} = foldMap f classNames
    <> foldMap (bifoldMap f g) relationships

instance Bitraversable ClassDiagram where
  bitraverse f g ClassDiagram {..} = ClassDiagram
    <$> traverse f classNames
    <*> traverse (bitraverse f g) relationships

type Cd = ClassDiagram String String

shuffleClassAndConnectionOrder :: MonadRandom m => Cd -> m Cd
shuffleClassAndConnectionOrder ClassDiagram {..} = ClassDiagram
  <$> shuffleM classNames
  <*> shuffleM relationships

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
  Letters <$> endBy (satisfy isAlpha) skipSpaces

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
  } deriving (Eq, Foldable, Functor, Generic, Read, Show, Traversable)

data ClassConfig = ClassConfig {
    classLimits               :: (Int, Int),
    aggregationLimits         :: (Int, Maybe Int),
    associationLimits         :: (Int, Maybe Int),
    compositionLimits         :: (Int, Maybe Int),
    inheritanceLimits         :: (Int, Maybe Int),
    -- | the number of relationships including inheritances
    relationshipLimits        :: (Int, Maybe Int)
  } deriving (Eq, Generic, Read, Show)

checkClassConfigWithProperties
  :: ClassConfig
  -> RelationshipProperties
  -> Maybe String
checkClassConfigWithProperties
  c@ClassConfig {..}
  RelationshipProperties {..}
  | wrongAssocs > maxRelations - fst inheritanceLimits
  || maybe False (wrongAssocs >) maxAssocs'
  = Just [iii|
    The (maximum) number of non-inheritance relationships is too low for
    the targeted wrongAssocs!
    |]
  | wrongCompositions > maxCompositions
  || maybe False (wrongCompositions >) (snd compositionLimits)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the targeted wrongCompositions!
    |]
  | minCompositions > maxCompositions
  || maybe False (minCompositions >) (snd compositionLimits)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the targeted composition properties!
    |]
  | minCompositionsInheritances > maxCompositionsInheritances
  || maybe False (minCompositionsInheritances >) maxCompositionsInheritances'
  = Just [iii|
    The (maximum) number of possible compositions or inheritances is too low for
    creating composition cycles!
    |]
  | minAssocs > maxRelations - fst inheritanceLimits
  || maybe False (minAssocs >) maxAssocs'
  = Just [iii|
    The (maximum) number of possible non-inheritance relationships is too low for
    the targeted non-inheritance relationship properties!
    |]
  | minInheritances > maxInheritances
  || maybe False (minInheritances >) (snd inheritanceLimits)
  = Just [iii|
    The (maximum) number of possible inheritance relationships is too low for
    the targeted inheritance relationship properties!
    |]
  | Just x <- snd relationshipLimits, Just rels <- relationshipsSum c, x > rels
  = Just [iii|
    The maximum number of relationships is too high
    according to individual relationship maxima!
    |]
  | x@Just {} <- snd relationshipLimits
  , any ((> x) . snd) [
      aggregationLimits,
      associationLimits,
      compositionLimits,
      inheritanceLimits
      ]
  = Just [iii|
    The maximum number of aggregations, associations, compositions
    as well as inheritances
    must not be higher than the maximum number of relationships!
    |]
  | otherwise = checkClassConfig c
  where
    for x y = if y then x else 0
    plusOne x = if x /= 0 then x + 1 else x
    minAssocs = (+ selfRelationships) . plusOne $ sum [
      1 `for` hasDoubleRelationships,
      1 `for` hasReverseRelationships
      ]
    minInheritances = (+ selfInheritances) . plusOne $ sum [
      1 `for` hasReverseInheritances,
      1 `for` hasMultipleInheritances,
      2 `for` hasNonTrivialInheritanceCycles
      ]
    minCompositions = max
      (1 `for` hasCompositionCycles)
      (2 `for` hasCompositionsPreventingParts)
    minCompositionsInheritances =
      3 `for` hasCompositionCycles
    maxRelations = fromMaybe (maxRels c) $ snd relationshipLimits
    maxCompositionsInheritances = maxRelations
      - fst aggregationLimits
      - fst associationLimits
    maxCompositions = maxCompositionsInheritances - fst inheritanceLimits
    maxInheritances = maxCompositionsInheritances - fst compositionLimits
    maxCompositionsInheritances' = (+)
      <$> snd compositionLimits
      <*> snd inheritanceLimits
    maxAssocs' = (\x y z -> x + y + z)
      <$> snd aggregationLimits
      <*> snd associationLimits
      <*> snd compositionLimits

checkClassConfig :: ClassConfig -> Maybe String
checkClassConfig c@ClassConfig {..} = checkRange Just "classLimits" classLimits
  <|> checkRange id "aggregationLimits" aggregationLimits
  <|> checkRange id "associationLimits" associationLimits
  <|> checkRange id "compositionLimits" compositionLimits
  <|> checkRange id "inheritanceLimits" inheritanceLimits
  <|> checkRange id "relationshipLimits" relationshipLimits
  <|> toMaybe (fst relationshipLimits < minRels c) [iii|
      The sum of the minimum number of aggregations, associations, compositions
      and inheritances
      must not be higher than the minimum number of relationships!
      |]
  <|> do
    void $ snd relationshipLimits
    toMaybe isMaxHigherThanAnyIndividual [iii|
      The maximum number of aggregations, associations, compositions
      as well as inheritances
      must not be higher than the maximum number of relationships!
      |]
  where
    toMaybe True x = Just x
    toMaybe _    _ = Nothing
    isMaxHigherThanAnyIndividual = any
      ((> snd relationshipLimits) . snd) [
        aggregationLimits,
        associationLimits,
        compositionLimits,
        inheritanceLimits
        ]

checkRange
  :: (Num n, Ord n, Show b, Show n)
  => (b -> Maybe n)
  -> String
  -> (n, b)
  -> Maybe String
checkRange g what (low, h) = do
  high <- g h
  assert high
  where
    assert high
      | low < 0 = Just @String [iii|
        The lower limit for #{what} has to be at least 0!
        |]
      | high < low = Just [iii|
        The upper limit (currently #{show h}; second value) for #{what}
        has to be as high as its lower limit
        (currently #{show low}; first value)!
        |]
      | otherwise = Nothing

minRels :: ClassConfig -> Int
minRels ClassConfig {..} =
  fst aggregationLimits
  + fst associationLimits
  + fst compositionLimits
  + fst inheritanceLimits

relationshipsSum :: ClassConfig -> Maybe Int
relationshipsSum ClassConfig {..} = sumOf4
  <$> snd aggregationLimits
  <*> snd associationLimits
  <*> snd compositionLimits
  <*> snd inheritanceLimits
  where
    sumOf4 w x y z = w + x + y + z

maxRels :: ClassConfig -> Int
maxRels config = fromMaybe (maxClasses * (maxClasses - 1) `div` 2)
  $ relationshipsSum config
  where
    maxClasses = snd $ classLimits config

{-|
Defines the size restrictions of an object diagram.
-}
data ObjectConfig = ObjectConfig {
  -- | lower and upper limit of links within the object diagram
  links             :: !(Int, Maybe Int),
  -- | lower and upper limit of links starting or ending at each object
  linksPerObject    :: !(Int, Maybe Int),
  -- | lower and upper limit of objects within the object diagram
  objects           :: !(Int, Int)
  } deriving (Eq, Generic, Read, Show)

{-|
Defines an 'ObjectConfig' demanding at least one but at most five objects
without restricting links.
-}
maxFiveObjects :: ObjectConfig
maxFiveObjects = ObjectConfig {
  links             = (0, Nothing),
  linksPerObject    = (0, Nothing),
  objects           = (1, 5)
  }

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
    hasCompositionsPreventingParts :: Bool,
    hasThickEdges           :: Maybe Bool
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
    hasCompositionsPreventingParts = False,
    hasThickEdges           = Nothing
  }

associationNames :: Cd -> [String]
associationNames = mapMaybe relationshipName . relationships

classNamesOd :: Od -> [String]
classNamesOd o = head . splitOn "$" <$> fst o

linkNames :: Od -> [String]
linkNames o = nub $ (\(_,_,x) -> x) `map` snd o

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

renameClassesAndRelationshipsInCd
  :: (MonadThrow m, Ord c, Ord c', Ord r, Ord r')
  => Bimap c c'
  -> Bimap r r'
  -> ClassDiagram c r
  -> m (ClassDiagram c' r')
renameClassesAndRelationshipsInCd cm rm =
  bitraverse (`BM.lookup` cm) (`BM.lookup` rm)

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
