{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Diagrams.

module Capabilities.Diagrams.IO () where

import qualified Data.ByteString.Lazy             as LBS (ByteString, unpack)
import qualified Data.Map                         as M (fromList)
import qualified Data.Text                        as T (
  Text,
  filter,
  isSuffixOf,
  length,
  pack,
  )
import qualified Data.Text.IO                     as T (writeFile)
import qualified Data.Text.Lazy                   as LT (Text, toStrict)
import qualified Graphics.SVGFonts.Fonts          (lin)

import Capabilities.Diagrams                      (MonadDiagrams (lin, writeSvg))
import Modelling.Auxiliary.Diagrams               (renderSVG)

import Data.ByteString.Internal                   (w2c)
import Diagrams.TwoD                              (dims2D)

import Text.XML.HXT.Core
    ( returnA,
      (>>>),
      (>>^),
      readString,
      runX,
      no,
      withInputEncoding,
      withMimeTypeFile,
      withRemoveWS,
      withStrictInput,
      withValidate,
      yes,
      isoLatin1,
      ArrowList(listA),
      ArrowTree(deep),
      ArrowXml(getAttrValue, isElem, hasName),
      IOStateArrow,
      XNode,
      XmlTree )

import Data.Tree.NTree.TypeDefs (NTree)

import qualified Text.XML as XML

import Data.List (isPrefixOf)
import Data.Maybe                       (maybeToList)

instance MonadDiagrams IO where
  lin = Graphics.SVGFonts.Fonts.lin
  writeSvg file g = do
    svg <- groupSVG $ renderSVG (dims2D 400 400) g
    T.writeFile file $ LT.toStrict svg

data SVGOptions = SVGOptions
  { xmlns, height, iStrokeOpacity, viewBox, fontSize, width, xmlnsXlink, iStroke, version :: T.Text,
    groups :: [SVGGroup] }
  deriving (Show, Eq)

data SVGGroup = SVGGroup
  { strokeLinejoin, strokeOpacity, fillOpacity, stroke, strokeWidth, fill, strokeLinecap, strokeMiterlimit, svgClass :: T.Text,
    paths :: [Path] }
  deriving (Show, Eq)

data Path = Path {
  d                 :: T.Text,
  pClass            :: T.Text,
  pFill             :: Maybe T.Text,
  pFillOpacity      :: T.Text,
  pStroke           :: Maybe T.Text,
  pStrokeWidth      :: Maybe T.Text
  }
  deriving (Show, Eq)

parseXML :: String -> IOStateArrow s b XmlTree
parseXML = readString [ withInputEncoding  isoLatin1
                        , withValidate       no
                        , withMimeTypeFile   "/etc/mime.types"
                        , withStrictInput    no
                        , withRemoveWS       yes
                        ]

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

getGroup :: ArrowXml cat => cat (NTree XNode) SVGGroup
getGroup = atTag "g" >>>
  proc g -> do
    strokeLinejoin   <- getAttrValue "stroke-linejoin"   >>^ T.pack -< g
    strokeOpacity    <- getAttrValue "stroke-opacity"    >>^ T.pack -< g
    fillOpacity      <- getAttrValue "fill-opacity"      >>^ T.pack -< g
    stroke           <- getAttrValue "stroke"            >>^ T.pack -< g
    strokeWidth      <- getAttrValue "stroke-width"      >>^ T.pack -< g
    fill             <- getAttrValue "fill"              >>^ T.pack -< g
    strokeLinecap    <- getAttrValue "stroke-linecap"    >>^ T.pack -< g
    strokeMiterlimit <- getAttrValue "stroke-miterlimit" >>^ T.pack -< g
    svgClass         <- getAttrValue "class"             >>^ T.pack -< g
    paths            <- listA getPath -< g
    returnA -< SVGGroup {..}

getPath :: ArrowXml cat => cat (NTree XNode) Path
getPath = atTag "path" >>>
  proc p -> do
    dAtt <- getAttrValue "d" -< p
    pFillAtt <- getAttrValue "fill" -< p
    pStrokeAtt <- getAttrValue "stroke" -< p
    pStrokeWidthAtt <- getAttrValue "stroke-width" -< p
    returnA -< Path {
      d = T.pack dAtt,
      pClass = "default",
      pFill = mText pFillAtt,
      pFillOpacity = "0",
      pStroke = mText pStrokeAtt,
      pStrokeWidth = mText pStrokeWidthAtt
      }
  where
    mText [] = Nothing
    mText xs = Just $ T.pack xs

getSVGAttributes :: ArrowXml cat => cat (NTree XNode) SVGOptions
getSVGAttributes = atTag "svg" >>>
  proc s -> do
    xmlns          <- getAttrValue "xmlns"          >>^ T.pack -< s
    height         <- getAttrValue "height"         >>^ T.pack -< s
    iStrokeOpacity <- getAttrValue "stroke-opacity" >>^ T.pack -< s
    viewBox        <- getAttrValue "viewBox"        >>^ T.pack -< s
    fontSize       <- getAttrValue "font-size"      >>^ T.pack -< s
    width          <- getAttrValue "width"          >>^ T.pack -< s
    xmlnsXlink     <- getAttrValue "xmlns:xlink"    >>^ T.pack -< s
    iStroke        <- getAttrValue "stroke"         >>^ T.pack -< s
    version        <- getAttrValue "version"        >>^ T.pack -< s
    groups         <- listA getGroup -< s
    returnA -< SVGOptions {..}

applyClass :: SVGGroup -> [Path]
applyClass x = [ modify p | p <- paths x]
  where
    modify p = p {
      pClass = T.filter (/='.') (svgClass x),
      pFill = mString $ fill x,
      pFillOpacity = fillOpacity x,
      pStroke = mString $ stroke x,
      pStrokeWidth = mString $ strokeWidth x
      }
    mString [] = Nothing
    mString xs = Just xs

formatSVG :: [SVGGroup] -> [SVGGroup]
formatSVG []     = []
formatSVG (x:xs) = x{ paths = groupPaths } : formatSVG rest
                where
                  groupPaths
                    | isLabelOrEdge x = concatMap applyClass (filter (equalGroup (svgClass x) . svgClass) (x:xs))
                    | otherwise = applyClass x ++ concatMap applyClass (takeWhile (equalGroup (svgClass x) . svgClass) xs)
                  rest
                    | isLabelOrEdge x = filter (not . equalGroup (svgClass x) . svgClass) xs
                    | otherwise = dropWhile (equalGroup (svgClass x) . svgClass) xs
                  isLabelOrEdge z = let fx = T.filter (/='.') (svgClass z) in fx == "elabel" || fx == "edge"

equalGroup :: T.Text -> T.Text -> Bool
equalGroup x y
  | (fx == "edge" || fx == "elabel") && (fy == "elabel" || fy == "edge") && sameLength = True
  | fx == "node"
  , fy /= "node"
  , fy == "token" || fy == "nlabel" || "node" `T.isSuffixOf` fy = True
  | (fx == "rect") && (fy == "") = True
  | otherwise = False
  where fx = T.filter (/= '.') x
        fy = T.filter (/= '.') y
        sameLength = T.length (T.filter (== '.') x) == T.length (T.filter (== '.') y)

buildSVG :: SVGOptions -> XML.Element
buildSVG svg = XML.Element "svg" [
  ("xmlns", xmlns svg),
  ("height", height svg),
  ("stroke-opacity", iStrokeOpacity svg),
  ("viewBox", viewBox svg),
  ("font-size", fontSize svg),
  ("width", width svg),
  ("xmlns:xlink", xmlnsXlink svg),
  ("stroke", iStroke svg),
  ("version", version svg)
  ]
  [ XML.NodeElement $ XML.Element "g" [
      ("stroke-linejoin", strokeLinejoin gr),
      ("stroke-opacity", strokeOpacity gr),
      ("fill-opacity", fillOpacity gr),
      ("stroke", stroke gr),
      ("stroke-width", strokeWidth gr),
      ("fill", fill gr),
      ("stroke-linecap", strokeLinecap gr),
      ("stroke-miterlimit", strokeMiterlimit gr)
      ]
  [ XML.NodeElement $ XML.Element "path" (M.fromList $
      [("d", d ps), ("class", renameClass $ pClass ps), ("fill-opacity", pFillOpacity ps)]
      ++ [("fill", x)
         | pFillOpacity ps /= "0.0", x <- maybeToList $ pFill ps, x /= fill gr]
      ++ [("fill", "none") | pFillOpacity ps == "0.0"]
      ++ [("stroke", x) | x <- maybeToList $ pStroke ps, x /= stroke gr]
      ++ [("stroke-width", x) | x <- maybeToList $ pStrokeWidth ps, x /= strokeWidth gr]
      )
    []
  | ps <- paths gr] | gr <- groups svg ]
  where
    renameClass l = case l of
      "labelbgnode" -> "nlabel"
      _ -> l


removeDoctype :: Bool -> String -> String
removeDoctype _ "" = ""
removeDoctype False s@(c:cs)
  | "<!DOCTYPE" `isPrefixOf` s = removeDoctype True (drop 9 s)
  | otherwise = c : removeDoctype False cs
removeDoctype True (c:cs)
  | c == '>' = cs
  | otherwise = removeDoctype True cs

groupSVG :: LBS.ByteString -> IO LT.Text
groupSVG s' = do
  let s = map w2c $ LBS.unpack s'
  (x:_) <- runX (parseXML (removeDoctype False s) >>> getSVGAttributes)
  return $ XML.renderText XML.def $ XML.Document
    (XML.Prologue [] Nothing [])
    (buildSVG x{groups = formatSVG (groups x)})
    []
