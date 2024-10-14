{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.CdAndChanges.Instance (
  AnnotatedChangeAndCd (..),
  ChangeAndCd (..),
  ClassDiagramInstance,
  GenericClassDiagramInstance (..),
  ReadObjectDiagramFromAlloyException (..),
  UnexpectedRelation (..),
  fromInstance,
  fromInstanceWithNameOverlap,
  fromInstanceWithPredefinedNames,
  nameClassDiagramInstance,
  uniformlyAnnotateChangeAndCd,
  validChangeClassDiagram,
  ) where

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map                         as M (
  lookup,
  )
import qualified Data.Set                         as S (
  findMin,
  size,
  toList,
  )

import Modelling.Auxiliary.Common       (Object (Object, oName), toMap)
import Modelling.CdOd.Types (
  Annotation (..),
  AnyClassDiagram (..),
  AnyRelationship,
  ClassDiagram,
  InvalidRelationship (..),
  LimitedLinking (..),
  Relationship (..),
  anyRelationshipName,
  toValidCd,
  )
import Modelling.Types                  (Change (..))

import Control.Monad                    ((<=<), forM)
import Control.Monad.Catch              (Exception, MonadThrow (throwM))
import Data.Bifunctor                   (Bifunctor (bimap, second))
import Data.Bifunctor.TH (
  deriveBifoldable,
  deriveBifunctor,
  deriveBitraversable,
  )
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (Bitraversable (bitraverse))
import Data.Composition                 ((.:))
import Data.Map                         (Map)
import Data.Maybe (
  fromMaybe,
  isJust,
  mapMaybe,
  maybeToList,
  )
import Data.Typeable                    (Typeable)
import Language.Alloy.Call (
  AlloyInstance,
  AlloySig,
  getDoubleAs,
  getSingleAs,
  lookupSig,
  scoped,
  )

objectName :: Object -> String
objectName (Object n x) = n ++ '$' : show x

newtype NumberedClass = NumberedClass Int
  deriving (Eq, Ord)

data NumberedNonInheritance = NumberedNonInheritance String Int
  deriving (Eq, Ord)

data AnnotatedChangeAndCd annotation className relationshipName
  = AnnotatedChangeAndCd {
    annotatedRelationshipChange
      :: !(Annotation
        annotation
        (Change (AnyRelationship className relationshipName))),
    annotatedChangeClassDiagram
      :: !(AnyClassDiagram className relationshipName)
    }
  deriving (Read, Show)

data ChangeAndCd className relationshipName
  = ChangeAndCd {
    relationshipChange
      :: !(Change (AnyRelationship className relationshipName)),
    changeClassDiagram
      :: !(AnyClassDiagram className relationshipName)
    }
  deriving (Read, Show)

instance Functor (ChangeAndCd className) where
  fmap f ChangeAndCd {..} = ChangeAndCd {
    relationshipChange = fmap (bimap (fmap f) (fmap f)) relationshipChange,
    changeClassDiagram = fmap f changeClassDiagram
    }

$(deriveBifunctor ''ChangeAndCd)
$(deriveBifoldable ''ChangeAndCd)
$(deriveBitraversable ''ChangeAndCd)

validChangeClassDiagram
  :: (
    Eq className,
    MonadThrow m,
    Show className,
    Show relationshipName,
    Typeable className,
    Typeable relationshipName
    )
  => ChangeAndCd className relationshipName
  -> m (ClassDiagram className relationshipName)
validChangeClassDiagram = toValidCd . changeClassDiagram

uniformlyAnnotateChangeAndCd
  :: annotation
  -> ChangeAndCd className relationshipName
  -> AnnotatedChangeAndCd annotation className relationshipName
uniformlyAnnotateChangeAndCd annotation ChangeAndCd {..} = AnnotatedChangeAndCd {
  annotatedRelationshipChange = Annotation {
    annotated = relationshipChange,
    annotation = annotation
    },
  annotatedChangeClassDiagram = changeClassDiagram
  }

data GenericClassDiagramInstance className relationshipName
  = ClassDiagramInstance {
    instanceClassDiagram      :: !(AnyClassDiagram className relationshipName),
    instanceRelationshipNames :: [relationshipName],
    instanceChangesAndCds     :: [ChangeAndCd className relationshipName]
    }
  deriving (Functor, Read, Show)

instance Bifunctor GenericClassDiagramInstance where
  bimap f g ClassDiagramInstance {..} = ClassDiagramInstance {
    instanceClassDiagram = bimap f g instanceClassDiagram,
    instanceRelationshipNames = map g instanceRelationshipNames,
    instanceChangesAndCds = map (bimap f g) instanceChangesAndCds
    }

instance Bifoldable GenericClassDiagramInstance where
  bifoldMap f g ClassDiagramInstance {..} = bifoldMap f g instanceClassDiagram
    <> foldMap g instanceRelationshipNames
    <> foldMap (bifoldMap f g) instanceChangesAndCds

instance Bitraversable GenericClassDiagramInstance where
  bitraverse f g ClassDiagramInstance {..} = ClassDiagramInstance
    <$> bitraverse f g instanceClassDiagram
    <*> traverse g instanceRelationshipNames
    <*> traverse (bitraverse f g) instanceChangesAndCds

type ClassDiagramInstance = GenericClassDiagramInstance String String

renameClassesAndRelationshipsInCdInstance
  :: (MonadThrow m, Ord c, Ord c', Ord r, Ord r')
  => Bimap c c'
  -> Bimap r r'
  -> GenericClassDiagramInstance c r
  -> m (GenericClassDiagramInstance c' r')
renameClassesAndRelationshipsInCdInstance
  bmClassNames
  bmRelationshipNames
  = bitraverse (`BM.lookup` bmClassNames) (`BM.lookup` bmRelationshipNames)

{-|
This version deliberately reuses names when changes to class diagrams
have been applied,
i.e. in the resulting class diagram the added relationship is named exactly
as the removed one.
This is especially required for the MatchCdOd task type where the overlap on
resulting ODs is intended.
Beware that this overlap is reflected in the class diagram only,
but NOT in the change itself.
-}
fromInstanceWithNameOverlap
  :: MonadThrow m
  => AlloyInstance
  -> m ClassDiagramInstance
fromInstanceWithNameOverlap alloyInstance = do
  cdInstance <- fromInstance alloyInstance
  return $ cdInstance {
    instanceChangesAndCds = map deliberatelyNameReplacedEdgesSameInCdOnly
      $ instanceChangesAndCds cdInstance
    }
  where
    deliberatelyNameReplacedEdgesSameInCdOnly change =
      case relationshipChange change of
        Change {add = Just rx, remove = Just ry}
          | Just x <- anyRelationshipName rx
          , Just y <- anyRelationshipName ry ->
            let rename = second (\x' -> if x' == x then y else x')
            in change {
              changeClassDiagram = rename $ changeClassDiagram change
              }
        Change {} -> change

{-|
Retrieve the instance with predefined class diagram component names.
This only makes sense if a class diagram with names was already provided to
Alloy beforehand.

This is achieved by relying on 'usePredefinedClassDiagramInstanceNames';
be sure to check its restrictions!
-}
fromInstanceWithPredefinedNames
  :: MonadThrow m
  => AlloyInstance
  -> m (GenericClassDiagramInstance String String)
fromInstanceWithPredefinedNames =
  usePredefinedClassDiagramInstanceNames <=< fromInstanceWithNameOverlap

{-|
Parses an class diagram instance from Alloy consisting of a base CD,
relationship names and changes with CDs derived from the base CD.
-}
fromInstance
  :: MonadThrow m
  => AlloyInstance
  -> m ClassDiagramInstance
fromInstance alloyInstance = do
  es <- instanceToEdges alloyInstance
  cs <- instanceToChanges alloyInstance
  namesOfClasses <- instanceToNamesOf alloyInstance "Class"
  namesOfNonInheritances <- instanceToNamesOf alloyInstance "NonInheritance"
  let baseCd = AnyClassDiagram {
        anyClassNames = namesOfClasses,
        anyRelationships =
          [e | (o, e) <- es, o `notElem` mapMaybe add cs]
        }
      modifiedCd ma mr = baseCd {
        anyRelationships = maybeToList ma
          ++ maybe id (filter . (/=)) mr (anyRelationships baseCd)
        }
  return ClassDiagramInstance {
    instanceClassDiagram = baseCd,
    instanceRelationshipNames = namesOfNonInheritances,
    instanceChangesAndCds = [
        ChangeAndCd {
          relationshipChange = Change a r,
          changeClassDiagram = modifiedCd a r
          }
      | c <- cs
      , a <- lookupM (add c) es
      , r <- lookupM (remove c) es
      ]
    }
  where
    lookupM :: Eq a => Maybe a -> [(a, b)] -> [Maybe b]
    lookupM Nothing  _  = [Nothing]
    lookupM (Just k) ms = [v | let v = lookup k ms, isJust v]

instanceToNamesOf
  :: MonadThrow m
  => AlloyInstance
  -> String
  -> m [String]
instanceToNamesOf alloyInstance what = do
  x <- lookupSig (scoped "this" what) alloyInstance
  map objectName . S.toList <$> getSingleAs "" (return .: Object) x

instanceToChanges
  :: MonadThrow m
  => AlloyInstance
  -> m [Change Object]
instanceToChanges alloyInstance = do
  c'      <- lookupSig (scoped "this" "Change") alloyInstance
  cs      <- S.toList <$> getSingleAs "" (return .: Object) c'
  cAdd    <- getRelation "add" c'
  cRemove <- getRelation "remove" c'
  return $ map (change cAdd cRemove) cs
  where
    change cAdd cRemove c =
      Change (M.lookup c cAdd) (M.lookup c cRemove)

newtype Relation = Relation {unRelation :: String}

newtype UnexpectedRelation
  = SingleMemberExpected Relation

instance Show UnexpectedRelation where
  show (SingleMemberExpected relation) = "SingleMemberExpected: "
    ++ "Relation " ++ unRelation relation
    ++ " matches at least one "
    ++ "member of its domain to multiple values of the codomain."

instance Exception UnexpectedRelation

getRelation :: MonadThrow m => String -> AlloySig -> m (Map Object Object)
getRelation n i = getDoubleAs n (return .: Object) (return .: Object) i
  >>= mapM single . toMap
  where
    single x
      | S.size x == 1 = return $ S.findMin x
      | otherwise     = throwM $ SingleMemberExpected $ Relation n

{-|
Parses all Relationships.
-}
instanceToEdges
  :: MonadThrow m
  => AlloyInstance
  -> m [(Object, AnyRelationship String String)]
instanceToEdges inst = do
  r'         <- lookupSig (scoped "this" "Relationship") inst
  rFrom      <- getRelation "from" r'
  rTo        <- getRelation "to" r'
  limited    <- lookupSig (scoped "this" "Limited") inst
  aFromLower <- getRelation "fromLower" limited
  aFromUpper <- getRelation "fromUpper" limited
  aToLower   <- getRelation "toLower" limited
  aToUpper   <- getRelation "toUpper" limited
  instanceToEdges' inst rFrom rTo aFromLower aFromUpper aToLower aToUpper

instanceToEdges'
  :: MonadThrow m
  => AlloyInstance
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> m [(Object, AnyRelationship String String)]
instanceToEdges' alloyInstance rFrom rTo aFromLower aFromUpper aToLower aToUpper = do
  inheritances <- getInheritances
  compositions <- getRelationships toComposition "Composition"
  aggregations <- getRelationships toAggregation "Aggregation"
  associations <- getRelationships toAssociation "Association"
  invalidInheritances <- map (second Left)
    <$> getRelationships toInvalidInheritance "InvalidInheritance"
  return . (++ invalidInheritances) . map (second Right)
    $ inheritances ++ compositions ++ aggregations ++ associations
  where
    getInheritances = do
      inheritance' <- lookupSig (scoped "this" "ValidInheritance") alloyInstance
      inheritances <-
        S.toList <$> getSingleAs "" (return .: Object) inheritance'
      forM inheritances $ \inheritance -> (inheritance,) <$> do
        from <- lookupObj inheritance rFrom
        to   <- lookupObj inheritance rTo
        return Inheritance {
          subClass = from,
          superClass = to
          }
    getRelationships f relationshipKind = do
      relationship' <- lookupSig (scoped "this" relationshipKind) alloyInstance
      relationships' <-
        S.toList <$> getSingleAs "" (return .: Object) relationship'
      forM relationships' $ \relationship -> (relationship,) <$> do
        let name = objectName relationship
        from <- getFrom relationship
        to   <- getTo relationship
        return $ f name from to
    getFrom = getLinking rFrom aFromLower aFromUpper
    getTo = getLinking rTo aToLower aToUpper
    toAssociation name from to = Association {
      associationName = name,
      associationFrom = from,
      associationTo = to
      }
    toAggregation name from to = Aggregation {
      aggregationName = name,
      aggregationPart = from,
      aggregationWhole = to
      }
    toComposition name from to = Composition {
      compositionName = name,
      compositionPart = from,
      compositionWhole = to
      }
    toInvalidInheritance _ from to = InvalidInheritance {
      invalidSubClass = from,
      invalidSuperClass = to
      }

data ReadObjectDiagramFromAlloyException
  = MissingObject
  | MissingLimit
  | UnknownLimit !String
  deriving Show

instance Exception ReadObjectDiagramFromAlloyException

lookupObj :: (MonadThrow m, Ord k) => k -> Map k Object -> m String
lookupObj k m = case M.lookup k m of
  Nothing -> throwM MissingObject
  Just v  -> pure $ objectName v

getLinking
  :: (MonadThrow m, Ord k)
  => Map k Object
  -> Map k Object
  -> Map k Object
  -> k
  -> m (LimitedLinking String)
getLinking link low high x = do
  link' <- lookupObj x link
  low'  <- lookupLimit x low
  high' <- lookupLimit x high
  return LimitedLinking {
    linking = link',
    limits = (fromMaybe (-1) low', high')
    }
  where
    lookupLimit k m = case M.lookup k m of
      Nothing -> throwM MissingLimit
      Just o -> case oName o of
        "Star" -> pure Nothing
        "Zero" -> pure $ Just 0
        "One"  -> pure $ Just 1
        "Two"  -> pure $ Just 2
        l      -> throwM $ UnknownLimit l

{-|
Define fresh names for each class diagram component.
Capital letters beginning from 'A' are used for class names.
Small letters beginning from 'z' backwards are used for relationship names.
-}
nameClassDiagramInstance
  :: (MonadThrow m, Ord className, Ord relationshipName)
  => GenericClassDiagramInstance className relationshipName
  -> m (GenericClassDiagramInstance String String)
nameClassDiagramInstance cdInstance =
  let cd = instanceClassDiagram cdInstance
      cs = anyClassNames cd
      es = instanceRelationshipNames cdInstance
      bimapEdges = BM.fromList $ zip es $ map (:[]) ['z', 'y' ..]
      bimapClasses = BM.fromList $ zip cs $ map (:[]) ['A' ..]
  in renameClassesAndRelationshipsInCdInstance
    bimapClasses
    bimapEdges
    cdInstance

{-|
Use predefined class diagram component names.
This only makes sense if a class diagram with names was already provided to
Alloy beforehand.

All names are gained by stripping everything after the dollar sign.
Attention! This is unsafe if new class diagram components are introduced,
e.g. as part of `add`.
-}
usePredefinedClassDiagramInstanceNames
  :: MonadThrow m
  => GenericClassDiagramInstance String String
  -> m (GenericClassDiagramInstance String String)
usePredefinedClassDiagramInstanceNames cdInstance =
  let cd = instanceClassDiagram cdInstance
      cs = anyClassNames cd
      es = instanceRelationshipNames cdInstance
      prefixOnly = takeWhile (/= '$')
      bimapEdges = BM.fromList $ zip es $ map prefixOnly es
      bimapClasses = BM.fromList $ zip cs $ map prefixOnly cs
  in renameClassesAndRelationshipsInCdInstance
    bimapClasses
    bimapEdges
    cdInstance
