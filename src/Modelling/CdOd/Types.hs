{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Modelling.CdOd.Types (
  AllowedProperties (..),
  AnnotatedCd,
  AnnotatedClassDiagram (..),
  Annotation (..),
  AnyCd,
  AnyClassDiagram (..),
  AnyRelationship,
  Cd,
  CdConstraints (..),
  CdDrawSettings (..),
  CdMutation (..),
  ClassConfig (..),
  ClassDiagram (..),
  DefaultedLimitedLinking (..),
  InvalidRelationship (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Property (..),
  Relationship (..),
  RelationshipMutation (..),
  RelationshipProperties (..),
  WrongRelationshipException (..),
  allCdMutations,
  allowEverything,
  allowNothing,
  anonymiseObjects,
  anyAssociationNames,
  anyRelationshipName,
  anyThickEdge,
  associationNames,
  calculateThickAnyRelationships,
  checkCdConstraints,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfig,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  checkObjectProperties,
  checkOmittedDefaultMultiplicities,
  classNamesOd,
  defaultCdConstraints,
  defaultCdDrawSettings,
  defaultedLimitedLinking,
  defaultOmittedDefaultMultiplicities,
  defaultProperties,
  fromClassDiagram,
  isIllegal,
  isObjectDiagramRandomisable,
  linkNames,
  maxFiveObjects,
  maxObjects,
  maxRelationships,
  normaliseObjectDiagram,
  rangeWithDefault,
  relationshipName,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  shuffleAnnotatedClassAndConnectionOrder,
  shuffleCdNames,
  shuffleClassAndConnectionOrder,
  shuffleAnyClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  sortLimits,
  toPropertySet,
  toValidCd,
  towardsValidProperties,
  unannotateCd,
  -- * Phrasing
  ArticlePreference (..),
  NonInheritancePhrasing (..),
  PhrasingKind (..),
  toArticleToUse,
  toPhrasing,
  ) where


import qualified Data.Bimap                       as BM
import qualified Data.Set                         as S (fromList)

import Modelling.Auxiliary.Common       (lowerFirst)

import Control.Applicative              (Alternative ((<|>)))
import Control.Enumerable               (deriveEnumerable)
import Control.Enumerable.Values        (allValues)
import Control.Exception                (Exception)
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.Random             (MonadRandom)
import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.Bifunctor                   (Bifunctor (bimap, first, second))
import Data.Bifunctor.TH (
  deriveBifoldable,
  deriveBifunctor,
  deriveBitraversable,
  )
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (Bitraversable (bitraverse))
import Data.List                        ((\\), isPrefixOf, sort, stripPrefix)
import Data.List.Extra                  (nubOrd)
import Data.Maybe (
  catMaybes,
  fromJust,
  fromMaybe,
  isJust,
  isNothing,
  mapMaybe,
  )
import Data.Ratio                       (denominator, numerator)
import Data.Set                         (Set)
import Data.String.Interpolate          (iii)
import Data.Tuple.Extra                 (both, dupe)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

type Od = ObjectDiagram String String String

data Object objectName className
  = Object {
    isAnonymous               :: !Bool,
    objectName                :: objectName,
    objectClass               :: className
    }
  deriving (Eq, Functor, Generic, Ord, Read, Show)

instance Bifunctor Object where
  bimap f g Object {..} = Object {
    isAnonymous     = isAnonymous,
    objectName      = f objectName,
    objectClass     = g objectClass
    }

instance Bifoldable Object where
  bifoldMap f g Object {..} = f objectName
    <> g objectClass

instance Bitraversable Object where
  bitraverse f g Object {..} = Object
    isAnonymous
    <$> f objectName
    <*> g objectClass

{-|
A link connects two objects and has a name.
-}
data Link objectName linkName
  = Link {
    -- | how the link is called, indicating which relationship it belongs to
    linkName                  :: linkName,
    -- | the starting point of the link
    linkFrom                  :: objectName,
    -- | the end point of the link
    linkTo                    :: objectName
    }
  deriving (Eq, Functor, Generic, Ord, Read, Show)

instance Bifunctor Link where
  bimap f g Link {..} = Link {
    linkName      = g linkName,
    linkFrom      = f linkFrom,
    linkTo        = f linkTo
    }

instance Bifoldable Link where
  bifoldMap f g Link {..} = g linkName
    <> f linkFrom
    <> f linkTo

instance Bitraversable Link where
  bitraverse f g Link {..} = Link
    <$> g linkName
    <*> f linkFrom
    <*> f linkTo

{-|
The object diagram consists of objects and links between them.

Note, the order of both, links and objects,
might influence its visual appearance when drawn.
-}
data ObjectDiagram objectName className linkName
  = ObjectDiagram {
    -- | all objects belonging to the object diagram
    objects                   :: [Object objectName className],
    -- | all links belonging to the object diagram
    links                     :: [Link objectName linkName]
    }
  deriving (Eq, Functor, Generic, Ord, Read, Show)

instance Bifunctor (ObjectDiagram a) where
  bimap f g ObjectDiagram {..} = ObjectDiagram {
    objects         = map (second f) objects,
    links           = map (second g) links
    }

instance Bifoldable (ObjectDiagram a) where
  bifoldMap f g ObjectDiagram {..} = foldMap (bifoldMap mempty f) objects
    <> foldMap (bifoldMap mempty g) links

instance Bitraversable (ObjectDiagram a) where
  bitraverse f g ObjectDiagram {..} = ObjectDiagram
    <$> traverse (bitraverse pure f) objects
    <*> traverse (bitraverse pure g) links

{-|
Sort objects, and links of the 'ObjectDiagram'.
This enables better comparison (especially for test cases).
-}
normaliseObjectDiagram
  :: (Ord className, Ord linkName, Ord objectName)
  => ObjectDiagram objectName className linkName
  -> ObjectDiagram objectName className linkName
normaliseObjectDiagram ObjectDiagram {..} = ObjectDiagram {
  objects = sort objects,
  links = sort links
  }

shuffleObjectAndLinkOrder
  :: MonadRandom m
  => ObjectDiagram objectName className linkName
  -> m (ObjectDiagram objectName className linkName)
shuffleObjectAndLinkOrder ObjectDiagram {..} = ObjectDiagram
  <$> shuffleM objects
  <*> shuffleM links

{-|
The basic mutation operations.
-}
data RelationshipMutation
  = ChangeKind
  | ChangeLimit
  | Flip
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

deriveEnumerable ''RelationshipMutation

data CdMutation
  = AddRelationship
  | MutateRelationship !RelationshipMutation
  | RemoveRelationship
  deriving (Eq, Generic, Ord, Read, Show)

deriveEnumerable ''CdMutation

allCdMutations :: [CdMutation]
allCdMutations = concat allValues

checkCdMutations :: [CdMutation] -> Maybe String
checkCdMutations mutations
  | null mutations
  = Just [iii|At least one CdMutation has to be enabled.|]
  | x:_ <- mutations \\ allCdMutations
  = Just [iii|
    There are no duplications allowed for the configured cd mutations
    but #{show x} appears twice.
    |]
  | otherwise
  = Nothing

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
  deriving (Eq, Functor, Foldable, Generic, Ord, Read, Show, Traversable)

{-|
A variation of 'LimitedLinking' that can fallback to a default limit
and includes the range expression.
-}
data DefaultedLimitedLinking = DefaultedLimitedLinking {
  -- | lower and upper bounds
  defaultedLimits :: Maybe (Int, Maybe Int),
  -- | a string representing the range
  defaultedRange :: Maybe String,
  -- | the target to which it is linking
  defaultedLinking :: !String
  }

{-|
Return lower limit first, higher second.
-}
sortLimits
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> (DefaultedLimitedLinking, DefaultedLimitedLinking)
sortLimits limit1 limit2 = (lower, higher)
  where
    (lower, higher)
      | defaultedLimits limit1 <= defaultedLimits limit2
      = (limit1, limit2)
      | otherwise
      = (limit2, limit1)

{-|
Smart constructor for creating 'DefaultedLimitedLinking'
based on a default and a 'LimitedLinking'.
-}
defaultedLimitedLinking
  :: Maybe (Int, Maybe Int)
  -> LimitedLinking String
  -> DefaultedLimitedLinking
defaultedLimitedLinking defaultLimits LimitedLinking {..}
  = DefaultedLimitedLinking {
    defaultedLimits = justNotDefault defaultLimits limits,
    defaultedRange = rangeWithDefault defaultLimits limits,
    defaultedLinking = linking
    }

{-|
Nothing if default is hit, else 'Just' the value.
-}
justNotDefault :: Eq a => Maybe a -> a -> Maybe a
justNotDefault defaultValue value
  | Just value == defaultValue
  = Nothing
  | otherwise
  = Just value

{-|
A range expression as shown in class diagrams (or 'Nothing' if default is hit).
-}
rangeWithDefault
  :: Maybe (Int, Maybe Int)
  -- ^ the default
  -> (Int, Maybe Int)
  -- ^ range for which to return the range expression
  -> Maybe String
rangeWithDefault defaultValue = fmap range . justNotDefault defaultValue
  where
    range (l, Nothing) = show l ++ "..*"
    range (l, Just u)
      | l == -1   = "*.." ++ show u
      | l == u    = show l
      | otherwise = show l ++ ".." ++ show u

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
  deriving (Eq, Functor, Generic, Ord, Read, Show)

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

data InvalidRelationship className relationshipName
  = InvalidInheritance {
    invalidSubClass :: !(LimitedLinking className),
    invalidSuperClass :: !(LimitedLinking className)
    }
  deriving (Eq, Functor, Generic, Ord, Read, Show)

$(deriveBifunctor ''InvalidRelationship)
$(deriveBifoldable ''InvalidRelationship)
$(deriveBitraversable ''InvalidRelationship)

type AnyRelationship className relationshipName
  = Either
    (InvalidRelationship className relationshipName)
    (Relationship className relationshipName)

relationshipName :: Relationship c r -> Maybe r
relationshipName x = case x of
  Association {..} -> Just associationName
  Aggregation {..} -> Just aggregationName
  Composition {..} -> Just compositionName
  Inheritance {}   -> Nothing

invalidRelationshipName
  :: InvalidRelationship className relationshipName
  -> Maybe relationshipName
invalidRelationshipName = \case
  InvalidInheritance {} -> Nothing

anyRelationshipName
  :: AnyRelationship className relationshipName
  -> Maybe relationshipName
anyRelationshipName = either invalidRelationshipName relationshipName

isRelationshipValid
  :: Eq className
  => Relationship className relationshipName
  -> Bool
isRelationshipValid = \case
  Association {..} ->
    validLimit associationFrom && validLimit associationTo
  Aggregation {..} ->
    validLimit aggregationPart && validLimit aggregationWhole
  Composition {..} ->
    validLimit compositionPart && validComposition (limits compositionWhole)
  Inheritance {..} ->
    subClass /= superClass
  where
    validLimit = validLimit' . limits
    validLimit' (x, Nothing) = x >= 0
    validLimit' (x, Just y) = x >= 0 && y > 0 && y >= x
    validComposition (_, Nothing) = False
    validComposition limit@(_, Just y) = y <= 1 && validLimit' limit

data Annotation annotation annotated = Annotation {
  annotated                   :: annotated,
  annotation                  :: annotation
  }
  deriving (Eq, Foldable, Functor, Generic, Read, Show, Traversable)

$(deriveBifunctor ''Annotation)
$(deriveBifoldable ''Annotation)
$(deriveBitraversable ''Annotation)

data AnnotatedClassDiagram relationshipAnnotation className relationshipName
  = AnnotatedClassDiagram {
    annotatedClasses
      :: [className],
    annotatedRelationships
      :: [Annotation relationshipAnnotation (AnyRelationship className relationshipName)]
    }
  deriving (Eq, Generic, Read, Show)

instance Functor (AnnotatedClassDiagram relationshipAnnotation className) where
  fmap f AnnotatedClassDiagram {..} = AnnotatedClassDiagram {
    annotatedClasses = annotatedClasses,
    annotatedRelationships = map
      (fmap $ bimap (fmap f) (fmap f))
      annotatedRelationships
    }

$(deriveBifunctor ''AnnotatedClassDiagram)
$(deriveBifoldable ''AnnotatedClassDiagram)
$(deriveBitraversable ''AnnotatedClassDiagram)

unannotateCd
  :: AnnotatedClassDiagram relationshipAnnotation className relationshipName
  -> AnyClassDiagram className relationshipName
unannotateCd AnnotatedClassDiagram {..} = AnyClassDiagram {
  anyClassNames = annotatedClasses,
  anyRelationships = map annotated annotatedRelationships
  }

data ClassDiagram className relationshipName = ClassDiagram {
  classNames                  :: [className],
  relationships               :: [Relationship className relationshipName]
  }
  deriving (Eq, Functor, Generic, Read, Show)

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

data AnyClassDiagram className relationshipName = AnyClassDiagram {
  anyClassNames           :: ![className],
  anyRelationships        :: ![AnyRelationship className relationshipName]
  }
  deriving (Eq, Generic, Read, Show)

instance Functor (AnyClassDiagram className) where
  fmap f AnyClassDiagram {..} = AnyClassDiagram {
    anyClassNames = anyClassNames,
    anyRelationships = map (bimap (fmap f) (fmap f)) anyRelationships
    }

$(deriveBifunctor ''AnyClassDiagram)
$(deriveBifoldable ''AnyClassDiagram)
$(deriveBitraversable ''AnyClassDiagram)

fromClassDiagram
  :: ClassDiagram className relationshipName
  -> AnyClassDiagram className relationshipName
fromClassDiagram ClassDiagram {..} = AnyClassDiagram {
  anyClassNames = classNames,
  anyRelationships = map Right relationships
  }

type Cd = ClassDiagram String String
type AnyCd = AnyClassDiagram String String
type AnnotatedCd annotation = AnnotatedClassDiagram annotation String String

toValidCd
  :: (
    Eq className,
    MonadThrow m,
    Show className,
    Show relationshipName,
    Typeable className,
    Typeable relationshipName
    )
  => AnyClassDiagram className relationshipName
  -> m (ClassDiagram className relationshipName)
toValidCd AnyClassDiagram {..} = do
  relationships <- mapM toRelationship anyRelationships
  pure ClassDiagram {
    classNames = anyClassNames,
    ..
    }
  where
    toRelationship anyRelationship
      | Right r <- anyRelationship
      , isRelationshipValid r = pure r
      | otherwise = throwM $ UnexpectedInvalidRelationship anyRelationship

newtype WrongRelationshipException className relationshipName
  = UnexpectedInvalidRelationship (AnyRelationship className relationshipName)
  deriving Show

instance (
  Show className,
  Show relationshipName,
  Typeable className,
  Typeable relationshipName
  )
  => Exception (WrongRelationshipException className relationshipName)

shuffleAnnotatedClassAndConnectionOrder
  :: MonadRandom m
  => AnnotatedClassDiagram annotation classes relationships
  -> m (AnnotatedClassDiagram annotation classes relationships)
shuffleAnnotatedClassAndConnectionOrder AnnotatedClassDiagram {..} = AnnotatedClassDiagram
  <$> shuffleM annotatedClasses
  <*> shuffleM annotatedRelationships

shuffleAnyClassAndConnectionOrder
  :: MonadRandom m
  => AnyCd
  -> m AnyCd
shuffleAnyClassAndConnectionOrder AnyClassDiagram {..} = AnyClassDiagram
  <$> shuffleM anyClassNames
  <*> shuffleM anyRelationships

shuffleClassAndConnectionOrder :: MonadRandom m => Cd -> m Cd
shuffleClassAndConnectionOrder ClassDiagram {..} = ClassDiagram
  <$> shuffleM classNames
  <*> shuffleM relationships

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
  | isNothing hasDoubleRelationships
  = Just [iii|
    'hasDoubleRelationships' must not be set to 'Nothing'
    |]
  | isNothing hasReverseRelationships
  = Just [iii|
    'hasReverseRelationships' must not be set to 'Nothing'
    |]
  | isNothing hasMultipleInheritances
  = Just [iii|
    'hasMultipleInheritances' must not be set to 'Nothing'
    |]
  | isNothing hasCompositionsPreventingParts
  = Just [iii|
    'hasCompositionsPreventingParts' must not be set to 'Nothing'
    |]
  | wrongNonInheritances > maxRelations - fst inheritanceLimits
  || maybe False (wrongNonInheritances >) maxNonInheritances'
  = Just [iii|
    The (maximum) number of non-inheritance relationships is too low for
    the aimed at wrongNonInheritances!
    |]
  | wrongCompositions > maxCompositions
  || maybe False (wrongCompositions >) (snd compositionLimits)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the aimed at wrongCompositions!
    |]
  | minCompositions > maxCompositions
  || maybe False (minCompositions >) (snd compositionLimits)
  = Just [iii|
    The (maximum) number of possible compositions is too low for
    the aimed at composition properties!
    |]
  | minCompositionsInheritances > maxCompositionsInheritances
  || maybe False (minCompositionsInheritances >) maxCompositionsInheritances'
  = Just [iii|
    The (maximum) number of possible compositions or inheritances is too low for
    creating composition cycles!
    |]
  | minNonInheritances > maxRelations - fst inheritanceLimits
  || maybe False (minNonInheritances >) maxNonInheritances'
  = Just [iii|
    The (maximum) number of possible non-inheritance relationships is too low for
    the aimed at non-inheritance relationship properties!
    |]
  | minInheritances > maxInheritances
  || maybe False (minInheritances >) (snd inheritanceLimits)
  = Just [iii|
    The (maximum) number of possible inheritance relationships is too low for
    the aimed at inheritance relationship properties!
    |]
  | Just x <- snd relationshipLimits,
    Just relationships <- relationshipsSum c,
    x > relationships
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
    forMaybe x y
      | Just False == y = 0
      | otherwise = x
    plusOne x = if x /= 0 then x + 1 else x
    minNonInheritances = (+ selfRelationshipsAmount) . plusOne $ sum [
      1 `forMaybe` hasDoubleRelationships,
      1 `forMaybe` hasReverseRelationships
      ]
    minInheritances = (+ selfInheritancesAmount) . plusOne $ sum [
      1 `for` hasReverseInheritances,
      1 `forMaybe` hasMultipleInheritances,
      2 `for` hasNonTrivialInheritanceCycles
      ]
    minCompositions = max
      (1 `for` hasCompositionCycles)
      (2 `forMaybe` hasCompositionsPreventingParts)
    minCompositionsInheritances =
      3 `for` hasCompositionCycles
    maxRelations = fromMaybe (maxRelationships c) $ snd relationshipLimits
    maxCompositionsInheritances = maxRelations
      - fst aggregationLimits
      - fst associationLimits
    maxCompositions = maxCompositionsInheritances - fst inheritanceLimits
    maxInheritances = maxCompositionsInheritances - fst compositionLimits
    maxCompositionsInheritances' = (+)
      <$> snd compositionLimits
      <*> snd inheritanceLimits
    maxNonInheritances' = (\x y z -> x + y + z)
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
  <|> toMaybe (fst relationshipLimits < minRelationships c) [iii|
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

{-|
Additional structural constraints that should be applied to all class diagrams.
-}
newtype CdConstraints
  = CdConstraints {
    anyCompositionCyclesInvolveInheritances :: Maybe Bool
    -- ^ if composition cycles have to contain inheritances (@Just True@),
    -- must not contain inheritances (@Just False@),
    -- or could contain inheritances (@Nothing@)
    } deriving (Eq, Generic, Read, Show)

defaultCdConstraints :: CdConstraints
defaultCdConstraints = CdConstraints {
  anyCompositionCyclesInvolveInheritances = Nothing
  }

checkCdConstraints :: AllowedProperties -> CdConstraints -> Maybe String
checkCdConstraints AllowedProperties {..} CdConstraints {..}
  | not compositionCycles, isJust anyCompositionCyclesInvolveInheritances
  = Just [iii|
    Setting anyCompositionCyclesInvolveInheritances to True or False
    makes no sense when composition cycles are not allowed.
    |]
  | otherwise
  = Nothing

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

checkObjectDiagram
  :: Ord objectName
  => ObjectDiagram objectName className linkName
  -> Maybe String
checkObjectDiagram ObjectDiagram {..}
  | objectNames /= nubOrd objectNames
  = Just "Every objectName has to be unique across the whole object diagram!"
  | otherwise
  = Nothing
  where
    objectNames = map objectName objects

minRelationships :: ClassConfig -> Int
minRelationships ClassConfig {..} =
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

maxRelationships :: ClassConfig -> Int
maxRelationships config = fromMaybe (maxClasses * (maxClasses - 1) `div` 2)
  $ relationshipsSum config
  where
    maxClasses = snd $ classLimits config

{-|
These parameters influence the appearance of the class diagram when drawn.
-}
data CdDrawSettings
  = CdDrawSettings {
    -- | These defaults will be omitted
    omittedDefaults :: !OmittedDefaultMultiplicities,
    -- | When set to 'False' relationship names will be omitted
    printNames :: !Bool,
    -- | When set to 'False' association arrows will be omitted
    printNavigations :: !Bool
    }
  deriving (Eq, Generic, Read, Show)

defaultCdDrawSettings :: CdDrawSettings
defaultCdDrawSettings = CdDrawSettings {
  omittedDefaults = defaultOmittedDefaultMultiplicities,
  printNames = True,
  printNavigations = True
  }

checkCdDrawSettings :: CdDrawSettings -> Maybe String
checkCdDrawSettings CdDrawSettings {..} =
  checkOmittedDefaultMultiplicities omittedDefaults

{-|
Defines default multiplicities which should be omitted
when drawing the class diagram.
-}
data OmittedDefaultMultiplicities
  = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity :: !(Maybe (Int, Maybe Int)),
    associationOmittedDefaultMultiplicity :: !(Maybe (Int, Maybe Int)),
    compositionWholeOmittedDefaultMultiplicity :: !(Maybe (Int, Maybe Int))
    }
  deriving (Eq, Generic, Read, Show)

defaultOmittedDefaultMultiplicities :: OmittedDefaultMultiplicities
defaultOmittedDefaultMultiplicities = OmittedDefaultMultiplicities {
  aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
  associationOmittedDefaultMultiplicity = Just (0, Nothing),
  compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
  }

checkOmittedDefaultMultiplicities :: OmittedDefaultMultiplicities -> Maybe String
checkOmittedDefaultMultiplicities OmittedDefaultMultiplicities {..} =
  checkValidity aggregationWholeOmittedDefaultMultiplicity
  <|> checkValidity associationOmittedDefaultMultiplicity
  <|> checkCompositionValidity compositionWholeOmittedDefaultMultiplicity
  where
    checkCompositionValidity limit@(Just (_, Just upper))
      | upper > 1
      = Just "The upper composition default multiplicity must not be higher than 1"
      | otherwise
      = checkValidity limit
    checkCompositionValidity limit = checkValidity limit
    checkValidity Nothing = Nothing
    checkValidity (Just (lower, maybeUpper))
      | lower < 0
      = Just "The lower default multiplicity limit must not be negative."
      | Just upper <- maybeUpper, upper < 1
      = Just "The upper default multiplicity limit must not be lower than 1."
      | Just upper <- maybeUpper, upper < lower
      = Just [iii|
        The upper default multiplicity limit must not be lower than
        the lower limit.
        |]
      | otherwise
      = Nothing

{-|
Defines the size restrictions of an object diagram.
-}
data ObjectConfig = ObjectConfig {
  -- | lower and upper limit of links within the object diagram
  linkLimits                  :: !(Int, Maybe Int),
  -- | lower and upper limit of links starting or ending at each object
  linksPerObjectLimits        :: !(Int, Maybe Int),
  -- | lower and upper limit of objects within the object diagram
  objectLimits                :: !(Int, Int)
  } deriving (Eq, Generic, Read, Show)

{-|
Defines structural constraints of an object diagram.
-}
data ObjectProperties = ObjectProperties {
  -- | a proportion in the interval 0 to 1
  -- describing what ratio of the objects should be anonymous
  -- where 0 is meaning none and 1 is meaning all
  anonymousObjectProportion   :: !Rational,
  -- | if there is at least one object for each existing class
  completelyInhabited         :: !(Maybe Bool),
  -- | if the number of isolated objects should be restricted
  hasLimitedIsolatedObjects   :: !Bool,
  -- | if there are links between the same object
  hasSelfLoops                :: !(Maybe Bool),
  -- | if there is at least one link
  -- for every association, aggregation and composition
  usesEveryRelationshipName   :: !(Maybe Bool)
  } deriving (Eq, Generic, Read, Show)

checkObjectProperties :: ObjectProperties -> Maybe String
checkObjectProperties ObjectProperties {..}
  | numerator anonymousObjectProportion < 0
  = Just [iii|
    anonymousObjectProportion must be positive
    |]
  | numerator anonymousObjectProportion > denominator anonymousObjectProportion
  = Just [iii|
    anonymousObjectProportion must be in the interval [0..1]
    |]
  | otherwise
  = Nothing

{-|
Defines an 'ObjectConfig' demanding at least one but at most five objects
without restricting links.
-}
maxFiveObjects :: ObjectConfig
maxFiveObjects = maxObjects 5

{-|
Defines an 'ObjectConfig' demanding at least one but at most the given number of
objects without restricting links.
-}
maxObjects :: Int -> ObjectConfig
maxObjects x = ObjectConfig {
  linkLimits                  = (0, Nothing),
  linksPerObjectLimits        = (0, Nothing),
  objectLimits                = (1, x)
  }

data RelationshipProperties
  = RelationshipProperties {
    invalidInheritances     :: !Int,
    wrongNonInheritances    :: Int,
    wrongCompositions       :: Int,
    selfRelationshipsAmount :: Int,
    selfInheritancesAmount  :: Int,
    hasDoubleRelationships  :: Maybe Bool,
    hasReverseRelationships :: Maybe Bool,
    hasReverseInheritances  :: Bool,
    hasMultipleInheritances :: Maybe Bool,
    hasNonTrivialInheritanceCycles :: Bool,
    hasCompositionCycles    :: Bool,
    hasCompositionsPreventingParts :: Maybe Bool,
    hasThickEdges           :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultProperties :: RelationshipProperties
defaultProperties =
  RelationshipProperties {
    invalidInheritances     = 0,
    wrongNonInheritances    = 0,
    wrongCompositions       = 0,
    selfRelationshipsAmount = 0,
    selfInheritancesAmount  = 0,
    hasDoubleRelationships  = Just False,
    hasReverseRelationships = Just False,
    hasReverseInheritances  = False,
    hasMultipleInheritances = Just False,
    hasNonTrivialInheritanceCycles = False,
    hasCompositionCycles    = False,
    hasCompositionsPreventingParts = Just False,
    hasThickEdges           = Nothing
  }

towardsValidProperties :: RelationshipProperties -> RelationshipProperties
towardsValidProperties properties@RelationshipProperties {..} = properties {
  invalidInheritances = snd betterInvalidInheritances,
  wrongNonInheritances = snd betterWrongNonInheritances,
  wrongCompositions = snd betterWrongCompositions,
  selfInheritancesAmount = snd betterSelfInheritances,
  hasReverseInheritances = snd fixedReverseInheritances,
  hasNonTrivialInheritanceCycles = snd fixedNonTrivialInheritanceCycles,
  hasCompositionCycles = snd fixedCompositionCycles
  }
  where
    betterInvalidInheritances = hasBetter False invalidInheritances
    betterWrongNonInheritances =
      hasBetter (fst betterInvalidInheritances) wrongNonInheritances
    betterWrongCompositions =
      hasBetter (fst betterWrongNonInheritances) wrongCompositions
    betterSelfInheritances =
      hasBetter (fst betterWrongCompositions) selfInheritancesAmount
    fixedReverseInheritances =
      fixed (fst betterSelfInheritances) hasReverseInheritances
    fixedNonTrivialInheritanceCycles =
      fixed (fst fixedReverseInheritances) hasNonTrivialInheritanceCycles
    fixedCompositionCycles =
      fixed (fst fixedNonTrivialInheritanceCycles) hasCompositionCycles
    hasBetter True x = (True, x)
    hasBetter False x = if x > 0 then (True, x - 1) else (False, x)
    fixed True x = (True, x)
    fixed False x = (x, False)

data Property =
    CompositionCycles
  | DoubleRelationships
  | InheritanceCycles
  | InvalidInheritanceLimits
  | MultipleInheritances
  | ReverseInheritances
  | ReverseRelationships
  | SelfInheritances
  | SelfRelationships
  | WrongAssociationLimits
  | WrongCompositionLimits
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

isIllegal :: Property -> Bool
isIllegal x = case x of
  CompositionCycles -> True
  DoubleRelationships -> False
  InheritanceCycles -> True
  InvalidInheritanceLimits -> True
  MultipleInheritances -> False
  ReverseInheritances -> True
  ReverseRelationships -> False
  SelfInheritances -> True
  SelfRelationships -> False
  WrongAssociationLimits -> True
  WrongCompositionLimits -> True

{-|
Create a set of properties based on a 'RelationshipProperties' configuration.
-}
toPropertySet :: RelationshipProperties -> Set Property
toPropertySet RelationshipProperties {..} =
  S.fromList $ catMaybes [
    ifTrue hasCompositionCycles CompositionCycles,
    ifJustTrue hasDoubleRelationships DoubleRelationships,
    ifTrue hasNonTrivialInheritanceCycles InheritanceCycles,
    ifJustTrue hasMultipleInheritances MultipleInheritances,
    ifTrue hasReverseInheritances ReverseInheritances,
    ifJustTrue hasReverseRelationships ReverseRelationships,
    ifAny selfInheritancesAmount SelfInheritances,
    ifAny selfRelationshipsAmount SelfRelationships,
    ifAny invalidInheritances InvalidInheritanceLimits,
    ifAny wrongNonInheritances WrongAssociationLimits,
    ifAny wrongCompositions WrongCompositionLimits
    ]
  where
    ifAny x p = if x > 0 then Just p else Nothing
    ifTrue x p = if x then Just p else Nothing
    ifJustTrue Nothing _ = Nothing
    ifJustTrue (Just x) p = ifTrue x p

data AllowedProperties = AllowedProperties {
  compositionCycles           :: Bool,
  doubleRelationships         :: Bool,
  inheritanceCycles           :: Bool,
  invalidInheritanceLimits    :: Bool,
  reverseInheritances         :: Bool,
  reverseRelationships        :: Bool,
  selfInheritances            :: Bool,
  selfRelationships           :: Bool,
  wrongAssociationLimits      :: Bool,
  wrongCompositionLimits      :: Bool
  } deriving (Generic, Read, Show)

allowEverything :: AllowedProperties
allowEverything = AllowedProperties {
  compositionCycles           = True,
  doubleRelationships         = True,
  inheritanceCycles           = True,
  invalidInheritanceLimits    = True,
  reverseInheritances         = True,
  reverseRelationships        = True,
  selfInheritances            = True,
  selfRelationships           = True,
  wrongAssociationLimits      = True,
  wrongCompositionLimits      = True
  }

allowNothing :: AllowedProperties
allowNothing = AllowedProperties {
  compositionCycles           = False,
  doubleRelationships         = False,
  inheritanceCycles           = False,
  invalidInheritanceLimits    = False,
  reverseInheritances         = False,
  reverseRelationships        = False,
  selfInheritances            = False,
  selfRelationships           = False,
  wrongAssociationLimits      = False,
  wrongCompositionLimits      = False
  }

associationNames :: Cd -> [String]
associationNames = mapMaybe relationshipName . relationships

anyAssociationNames :: AnyCd -> [String]
anyAssociationNames = mapMaybe names .  anyRelationships
  where
    names = either invalidRelationshipName relationshipName

classNamesOd
  :: Ord className
  => ObjectDiagram objectName className linkName
  -> [className]
classNamesOd ObjectDiagram {..} = nubOrd $ map objectClass objects

linkNames
  :: Ord linkName
  => ObjectDiagram objectName className linkName
  -> [linkName]
linkNames ObjectDiagram {..} = nubOrd $ map linkName links

{-|
Given a collection of CDs use all used class and relationship names
and shuffle them respectively.
-}
shuffleCdNames
  :: (MonadRandom m, Traversable t, MonadThrow m)
  => t Cd
  -> m (t Cd)
shuffleCdNames cds = do
  let names = nubOrd $ concatMap classNames cds
      nonInheritances = nubOrd $ concatMap associationNames cds
  names' <- shuffleM names
  nonInheritances' <- shuffleM nonInheritances
  let bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCds = renameClassesAndRelationships bmNames bmNonInheritances
  mapM renameCds cds

{-|
Renaming 'AnnotatedClassDiagram'gs, `ClassDiagram`s and `Relationship`s
is possible using this function
-}
renameClassesAndRelationships
  :: (Bitraversable f, MonadThrow m, Ord c, Ord c', Ord r, Ord r')
  => Bimap c c'
  -> Bimap r r'
  -> f c r
  -> m (f c' r')
renameClassesAndRelationships cm rm =
  bitraverse (`BM.lookup` cm) (`BM.lookup` rm)

data RenameException
  = ObjectNameNotMatchingToObjectClass
  deriving Show

instance Exception RenameException

renameObjectsWithClassesAndLinksInOd
  :: (MonadThrow m, Ord linkNames, Ord linkNames')
  => Bimap String String
  -> Bimap linkNames linkNames'
  -> ObjectDiagram String String linkNames
  -> m (ObjectDiagram String String linkNames')
renameObjectsWithClassesAndLinksInOd bmClasses bmLinks ObjectDiagram {..} = do
  objects' <- traverse renameObject objects
  let bmObjects = BM.fromList
        $ zip (map objectName objects) (map objectName objects')
  links' <- traverse
    (bitraverse (`BM.lookup` bmObjects) (`BM.lookup` bmLinks))
    links
  return ObjectDiagram {
    objects = objects',
    links = links'
    }
  where
    renameObject Object {..} = do
      className' <- BM.lookup objectClass bmClasses
      case stripPrefix (lowerFirst objectClass) objectName of
        Just objectNamePostfix -> return Object {
          isAnonymous = isAnonymous,
          objectName = lowerFirst className' ++ objectNamePostfix,
          objectClass = className'
          }
        Nothing -> throwM ObjectNameNotMatchingToObjectClass

anonymiseObjects
  :: MonadRandom m
  => Rational
  -> ObjectDiagram className relationshipName linkName
  -> m (ObjectDiagram className relationshipName linkName)
anonymiseObjects proportion ObjectDiagram {..} = do
  let objectCount = length objects
      anonymous = round (fromIntegral objectCount * proportion)
  makeAnonymousList <- shuffleM
    $ take objectCount $ replicate anonymous True ++ repeat False
  return ObjectDiagram {
    links = links,
    objects = zipWith anonymise makeAnonymousList objects
    }
  where
    anonymise :: Bool -> Object a b -> Object a b
    anonymise anonymous o = o {isAnonymous = anonymous}

canShuffleClassNames :: ObjectDiagram String String linkNames -> Bool
canShuffleClassNames ObjectDiagram {..} =
  all (\Object {..} -> lowerFirst objectClass `isPrefixOf` objectName) objects

isObjectDiagramRandomisable
  :: ObjectDiagram String String linkNames
  -> Maybe String
isObjectDiagramRandomisable od
  | not $ canShuffleClassNames od
  = Just [iii|
      object names of the CD have to match to their class names
      (e.g., c1 for C or anyOne for AnyOne).
      |]
  | otherwise
  = Nothing

anyThickEdge :: Cd -> Bool
anyThickEdge = any fst . calculateThickRelationships

calculateThickAnyRelationships
  :: AnyCd
  -> [(Bool, AnyRelationship String String)]
calculateThickAnyRelationships AnyClassDiagram {..} =
  calculateThickRelationshipsHelper
  toRelationship
  anyClassNames
  anyRelationships
  where
    toRelationship = either (const Nothing) Just

calculateThickRelationships :: Cd -> [(Bool, Relationship String String)]
calculateThickRelationships ClassDiagram {..} =
  calculateThickRelationshipsHelper
  Just
  classNames
  relationships

calculateThickRelationshipsHelper
  :: (relationship -> Maybe (Relationship String String))
  -> [String]
  -> [relationship]
  -> [(Bool, relationship)]
calculateThickRelationshipsHelper toRelationship allClassNames allRelationships =
  map (first (isThick . toRelationship) . dupe) allRelationships
  where
    classesWithSubclasses = map (\name -> (name, subs [] name)) allClassNames
      where
        subs seen name
          | name `elem` seen = []
          | otherwise = name : concatMap
              (subs (name:seen) . subClass)
              (filter ((name ==) . superClass) inheritances)
    relevantRelationships = mapMaybe toRelationship allRelationships
    inheritances = filter
      (\case Inheritance {} -> True; _ -> False)
      relevantRelationships
    nonInheritancesBothWays = concatMap
      (map (both linking) . nonInheritanceBothWays)
      relevantRelationships
    isThick Nothing = False
    isThick (Just x) = isNonInheritanceThick x
    isNonInheritanceThick r = case r of
      Inheritance {} -> False
      Association {..} -> shouldBeThick
        (linking associationFrom)
        (linking associationTo)
        classesWithSubclasses
        nonInheritancesBothWays
      Aggregation {..} -> shouldBeThick
        (linking aggregationWhole)
        (linking aggregationPart)
        classesWithSubclasses
        nonInheritancesBothWays
      Composition {..} -> shouldBeThick
        (linking compositionWhole)
        (linking compositionPart)
        classesWithSubclasses
        nonInheritancesBothWays
    nonInheritanceBothWays Inheritance {} = []
    nonInheritanceBothWays Association {..} =
      [(associationFrom, associationTo), (associationTo, associationFrom)]
    nonInheritanceBothWays Aggregation {..} =
      [(aggregationPart, aggregationWhole), (aggregationWhole, aggregationPart)]
    nonInheritanceBothWays Composition {..} =
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

{-|
When indefinite articles can not be avoided completely
-}
data ArticlePreference
  = UseDefiniteArticleWherePossible
  -- ^ prefer definite articles
  | UseIndefiniteArticleEverywhere
  -- ^ always use indefinite articles
  deriving (Eq, Generic, Read, Show)

{-|
Convert 'ArticlePreference' directly to 'ArticleToUse' (without conditions).
-}
toArticleToUse :: ArticlePreference -> ArticleToUse
toArticleToUse = \case
  UseDefiniteArticleWherePossible -> DefiniteArticle
  UseIndefiniteArticleEverywhere -> IndefiniteArticle

{-|
How to phrase non inheritance relationships
-}
data NonInheritancePhrasing
  = ByDirection
  -- ^ refer in some way to start and end
  | ByName
  -- ^ refer to the name
  | Lengthy
  -- ^ Associations are phrased lengthy, others as 'ByDirection'

{-|
Choose 'NonInheritancePhrasing' according to parameters in this order
 * by name (first parameter)
 * by direction (second parameter)
 * otherwise lengthy
-}
toPhrasing :: Bool -> Bool -> NonInheritancePhrasing
toPhrasing byName withDir
  | byName = ByName
  | withDir = ByDirection
  | otherwise = Lengthy

{-|
For choosing a specific phrasing variation.
-}
data PhrasingKind
  = Denoted
  -- ^ refer to denoted multiplicities
  | Participations
  -- ^ describe how often objects can participate
