{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}
module Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  emptyArr,
  filterFirst,
  oneAndOther,
  redColor,
  underlinedLabel,
  ) where

import qualified Data.Set                         as S

import Modelling.Auxiliary.Common       (lowerFirst)
import Modelling.CdOd.Types (
  DefaultedLimitedLinking (..),
  Link (..),
  Object (..),
  ObjectDiagram (..),
  Od,
  )

import Language.Alloy.Call              as Alloy (
  AlloyInstance,
  getSingleAs,
  getDoubleAs,
  lookupSig,
  scoped,
  )
import Language.Alloy.Exceptions        (AlloyLookupFailed (..))

import Control.Monad.Catch              (MonadCatch (catch), MonadThrow (throwM))
import Data.GraphViz                    (X11Color (..))
import Data.GraphViz.Attributes.Complete (
  ArrowShape (..),
  ArrowType (..),
  Attribute (..),
  Label (HtmlLabel),
  openMod,
  toWColor,
  )
import Data.GraphViz.Attributes.HTML    as Html
  (Label, Format (..), Label (Text), TextItem (..))
import Data.List.Extra                  (nubOrd)
import Data.Text.Lazy                   (pack)
import Control.Monad.Trans.Random       (RandT, liftCatch)

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys

underlinedLabel :: String -> Attribute
underlinedLabel s = Label (HtmlLabel label)
  where
    label :: Html.Label
    label = Text [Format Underline [Str (pack s)]]

emptyArr :: ArrowType
emptyArr = AType [(openMod, Normal)]

redColor :: Attribute
redColor = Color [toWColor Red]

instance {-# OVERLAPPABLE #-} MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

{-|
Parses the Alloy object diagram instance.
-}
alloyInstanceToOd
  :: MonadCatch m
  => Maybe [String]
  -- ^ the super class set of all potential objects
  -- (all possible class names is also possible)
  --
  -- Only required for Alloy instances that were generated with
  -- @LinguisticReuse@ set to @ExtendsAnd FieldPlacement@,
  -- otherwise 'Nothing'
  -> [String]
  -- ^ all possible link names
  -> AlloyInstance
  -- ^ the alloy instance to parse
  -> m Od
alloyInstanceToOd maybeAllClassNames allLinkNames i = case maybeAllClassNames of
  Nothing -> do
    os <- lookupSig (scoped "this" "Object") i
    objects <- S.toList <$> getSingleAs "" toObject os
    links <- concat <$> mapM (getLink os) (nubOrd allLinkNames)
    return ObjectDiagram {..}
  Just allClassNames -> do
    os <- mapM (flip lookupSig i . scoped "this") (nubOrd allClassNames)
    objects <- S.toList . S.unions <$> mapM (getSingleAs "" toObject) os
    links <- concat <$> mapM (getLinks os) (nubOrd allLinkNames)
    return ObjectDiagram {..}
  where
    getLink os l = map (toLink l) . S.toList
      <$> getDoubleAs l oName oName os
    getLinks os l = map (toLink l) . S.toList . S.unions
      <$> mapM (ignoreMissingRelation . getDoubleAs l oName oName) os
    ignoreMissingRelation = flip catch $ \case
      LookupAlloyRelationFailed {} -> pure S.empty
      exception -> throwM exception
    oName x = return . toObjectName x
    toObjectName x y = lowerFirst x ++ if y == 0 then [] else show y
    toObject x y = return $ Object {
      isAnonymous = False,
      objectName = toObjectName x y,
      objectClass = x
      }
    toLink l (x, y) = Link {
      linkLabel = l,
      linkFrom = x,
      linkTo = y
      }

oneAndOther
  :: String
  -> String
  -> (DefaultedLimitedLinking, DefaultedLimitedLinking)
  -> (DefaultedLimitedLinking, DefaultedLimitedLinking)
oneAndOther linking1 linking2 (limit1, limit2) = (
  limit1 {defaultedLinking = linking1},
  limit2 {defaultedLinking = linking2}
  )
