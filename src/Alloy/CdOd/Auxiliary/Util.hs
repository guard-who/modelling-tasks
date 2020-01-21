module Alloy.CdOd.Auxiliary.Util (
  emptyArr, filterFirst, firstLower, firstUpper, redColor, underlinedLabel,
  ) where

import Data.Char                        (isUpper, toLower, toUpper)
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
import Data.Text.Lazy                   (pack)

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys

firstLower :: String -> String
firstLower (c:cs) | isUpper c = toLower c : cs
firstLower cs = cs

firstUpper :: String -> String
firstUpper (c:cs) = toUpper c : cs
firstUpper cs = cs

underlinedLabel :: String -> Attribute
underlinedLabel s = Label (HtmlLabel label)
  where
    label :: Html.Label
    label = Text [Format Underline [Str (pack s)]]

emptyArr :: ArrowType
emptyArr = AType [(openMod, Normal)]

redColor :: Attribute
redColor = Color [toWColor Red]
