module Util where

import Data.Char
import Data.GraphViz.Attributes.Complete as Dot
import Data.GraphViz.Attributes.HTML as HTML
import Data.Text.Lazy (pack)

firstLower :: String -> String
firstLower (c:cs) | isUpper c = toLower c : cs

firstUpper :: String -> String
firstUpper (c:cs) = toUpper c : cs

underlinedLabel :: String -> Dot.Attribute
underlinedLabel s = Label (HtmlLabel (Text [HTML.Format HTML.Underline [Str (pack s)]]))

emptyArr :: ArrowType
emptyArr = AType [(openMod, Normal)]
