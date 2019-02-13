module Util where

import Data.Char
import Data.GraphViz
import Data.GraphViz.Attributes.Complete as Dot
import Data.GraphViz.Attributes.HTML as HTML
import Data.Text.Lazy (pack)

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys

firstLower :: String -> String
firstLower (c:cs) | isUpper c = toLower c : cs

firstUpper :: String -> String
firstUpper (c:cs) = toUpper c : cs

underlinedLabel :: String -> Dot.Attribute
underlinedLabel s = Label (HtmlLabel (Text [HTML.Format HTML.Underline [HTML.Str (pack s)]]))

emptyArr :: ArrowType
emptyArr = AType [(openMod, Normal)]

redColor :: Dot.Attribute
redColor = Dot.Color [toWColor Red]
