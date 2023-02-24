{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  ClassDiagramInstance,
  GenericClassDiagramInstance (..),
  fromInstance,
  renameClassesAndRelationshipsInCdInstance,
  ) where

import qualified Data.Bimap                       as BM (lookup)
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
  ClassDiagram (..),
  Change (..),
  LimitedLinking (..),
  Relationship (..),
  )

import Control.Monad                    (forM)
import Control.Monad.Catch              (MonadThrow)
import Data.Bifunctor                   (Bifunctor (bimap))
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
#if __GLASGOW_HASKELL__ >= 808
import Data.String                      (IsString, fromString)
#endif
import Language.Alloy.Call (
  AlloyInstance,
  AlloySig,
  getDoubleAs,
  getSingleAs,
  lookupSig,
  scoped,
  )

#if __GLASGOW_HASKELL__ >= 808
instance IsString a => MonadFail (Either a) where
  fail = Left . fromString
#endif

objectName :: Object -> String
objectName (Object n x) = n ++ '$' : show x

newtype NumberedClass = NumberedClass Int
  deriving (Eq, Ord)

data NumberedAssoc = NumberedAssoc String Int
  deriving (Eq, Ord)

type ClassDiagramInstance = GenericClassDiagramInstance String String

data ChangeAndCd className relationshipName
  = ChangeAndCd {
    relationshipChange
      :: Change (Relationship className relationshipName),
    changeClassDiagram
      :: ClassDiagram className relationshipName
    }
  deriving (Read, Show)

instance Bifunctor ChangeAndCd where
  bimap f g ChangeAndCd {..} = ChangeAndCd {
    relationshipChange = fmap (bimap f g) relationshipChange,
    changeClassDiagram = bimap f g changeClassDiagram
    }

instance Bifoldable ChangeAndCd where
  bifoldMap f g ChangeAndCd {..} = foldMap (bifoldMap f g) relationshipChange
    <> bifoldMap f g changeClassDiagram

instance Bitraversable ChangeAndCd where
  bitraverse f g ChangeAndCd {..} = ChangeAndCd
    <$> traverse (bitraverse f g) relationshipChange
    <*> bitraverse f g changeClassDiagram

data GenericClassDiagramInstance className relationshipName
  = ClassDiagramInstance {
    instanceClassDiagram      :: ClassDiagram className relationshipName,
    instanceRelationshipNames :: [relationshipName],
    instanceChangesAndCds     :: [ChangeAndCd className relationshipName]
    }
  deriving (Read, Show)

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


fromInstance
  :: AlloyInstance
  -> Either String ClassDiagramInstance
fromInstance insta = do
  es <- instanceToEdges insta
  cs <- instanceToChanges insta
  namesOfClasses <- instanceToNamesOf insta "Class"
  namesOfAssocs <- instanceToNamesOf insta "Assoc"
  let baseCd = ClassDiagram {
        classNames = namesOfClasses,
        relationships =
          [e | (o, e) <- es, o `notElem` mapMaybe add cs]
        }
      modifiedCd ma mr = baseCd {
        relationships = maybeToList ma
          ++ maybe id (filter . (/=)) mr (relationships baseCd)
        }
  return ClassDiagramInstance {
    instanceClassDiagram = baseCd,
    instanceRelationshipNames = namesOfAssocs,
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
  :: AlloyInstance
  -> String
  -> Either String [String]
instanceToNamesOf insta what = do
  x <- lookupSig (scoped "this" what) insta
  map objectName . S.toList <$> getSingleAs "" (return .: Object) x

instanceToChanges
  :: AlloyInstance
  -> Either String [Change Object]
instanceToChanges insta = do
  c'      <- lookupSig (scoped "this" "Change") insta
  cs      <- S.toList <$> getSingleAs "" (return .: Object) c'
  cAdd    <- getRelation "add" c'
  cRemove <- getRelation "remove" c'
  return $ map (change cAdd cRemove) cs
  where
    change cAdd cRemove c =
      Change (M.lookup c cAdd) (M.lookup c cRemove)

getRelation :: String -> AlloySig -> Either String (Map Object Object)
getRelation n i = getDoubleAs n (return .: Object) (return .: Object) i
  >>= mapM single . toMap
  where
    single x
      | S.size x == 1 = return $ S.findMin x
      | otherwise     = fail $ "Relation " ++ n ++ " matches at least one"
        ++ "member of its domain to multiple values of the codomain."

instanceToEdges
  :: AlloyInstance
  -> Either String [(Object, Relationship String String)]
instanceToEdges insta = do
  r'         <- lookupSig (scoped "this" "Relationship") insta
  rFrom      <- getRelation "from" r'
  rTo        <- getRelation "to" r'
  a'         <- lookupSig (scoped "this" "Assoc") insta
  aFromLower <- getRelation "fromLower" a'
  aFromUpper <- getRelation "fromUpper" a'
  aToLower   <- getRelation "toLower" a'
  aToUpper   <- getRelation "toUpper" a'
  instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper

instanceToEdges'
  :: AlloyInstance
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Either String [(Object, Relationship String String)]
instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper = do
  inheritances <- getInheritances
  compositions <- getRelationships toComposition "Composition"
  aggregations <- getRelationships toAggregation "Aggregation"
  associations <- getRelationships toAssociation "Association"
  return $ inheritances ++ compositions ++ aggregations ++ associations
  where
    getInheritances = do
      inheritance' <- lookupSig (scoped "this" "Inheritance") insta
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
      relationship' <- lookupSig (scoped "this" relationshipKind) insta
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

lookupObj :: Ord k => k -> Map k Object -> Either String String
lookupObj k m = case M.lookup k m of
  Nothing -> Left "Missing object "
  Just v  -> Right $ objectName v

getLinking
  :: Ord k
  => Map k Object
  -> Map k Object
  -> Map k Object
  -> k
  -> Either String (LimitedLinking String)
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
      Nothing -> Left "Missing limit"
      Just o -> case oName o of
        "Star" -> Right Nothing
        "Zero" -> Right $ Just 0
        "One"  -> Right $ Just 1
        "Two"  -> Right $ Just 2
        l      -> Left $ "Unknown limit " ++ l
