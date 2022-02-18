module Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  emptyArr,
  filterFirst,
  getInstances,
  redColor, underlinedLabel,
  ) where

import qualified Data.Set                         as S

import Language.Alloy.Call              as Alloy (
  AlloyInstance,
  CallAlloyConfig (..),
  defaultCallAlloyConfig,
  getInstancesWith,
  getSingle,
  getTriple,
  lookupSig,
  objectName,
  scoped,
  )

import Control.Monad.Trans.Except       (ExceptT, except)
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
import Data.List                        (elemIndex)
import Data.Maybe                       (fromJust)
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

getInstances :: Maybe Integer -> Maybe Int -> String -> IO [AlloyInstance]
getInstances mmaxInstances mtimeout = getInstancesWith $ defaultCallAlloyConfig {
  maxInstances = mmaxInstances,
  timeout      = mtimeout
  }

alloyInstanceToOd
  :: Monad m
  => AlloyInstance
  -> ExceptT String m ([String], [(Int, Int, String)])
alloyInstanceToOd i = except $ do
  os    <- lookupSig (scoped "this" "Obj") i
  objs  <- map objectName . S.toList <$> getSingle "" os
  links <- map (linkOf objs) . S.toList <$> getTriple "get" os
  return (objs, links)
  where
    nameOf   = takeWhile (/= '$') . objectName
    linkOf objs (x, l, y) =
      let indexOf z = fromJust $ elemIndex (objectName z) objs
      in (indexOf x, indexOf y, nameOf l)
