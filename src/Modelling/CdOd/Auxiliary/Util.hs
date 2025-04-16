{-# LANGUAGE RecordWildCards #-}
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

import Control.Monad.Catch              (MonadThrow)
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

{-|
Parses the Alloy object diagram instance.
-}
alloyInstanceToOd
  :: MonadThrow m
  => [String]
  -- ^ all possible link names
  -> AlloyInstance
  -- ^ the alloy instance to parse
  -> m Od
alloyInstanceToOd allLinkNames i = do
  os    <- lookupSig (scoped "this" "Object") i
  objects <- S.toList <$> getSingleAs "" toObject os
  links <- concat <$> mapM (getLink os) (nubOrd allLinkNames)
  return ObjectDiagram {..}
  where
    getLink os l = map (toLink l) . S.toList
      <$> getDoubleAs l oName oName os
    oName x = return . toObjectName x
    toObjectName x y = lowerFirst x ++ if y == 0 then [] else show y
    toObject x y = return $ Object {
      isAnonymous = False,
      objectName = toObjectName x y,
      objectClass = x
      }
    toLink l (x, y) = Link {
      linkName = l,
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
