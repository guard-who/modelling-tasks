{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, OverloadedLists #-}
-- | Defines a Monad context for rendering diagrams graphics to file.

module Capabilities.Diagrams (
  MonadDiagrams (lin, writeSvg),
  ) where

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

import Modelling.Auxiliary.Diagrams               (renderSVG)

import Data.ByteString.Internal                   (w2c)
import Data.Data                                  (Typeable)
import Diagrams.Backend.SVG                       (SVG)
import Diagrams.Prelude                           (QDiagram)
import Diagrams.TwoD                              (V2, dims2D)
import Graphics.SVGFonts.ReadFont                 (PreparedFont)

import Text.XML.HXT.Core
    ( returnA,
      (>>>),
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

class Monad m => MonadDiagrams m where
  lin :: (Read n, RealFloat n) => m (PreparedFont n)
  writeSvg
    :: (Show n, Typeable n, RealFloat n, Monoid o)
    => FilePath
    -> QDiagram SVG V2 n o
    -> m ()

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
    sljoin      <- getAttrValue "stroke-linejoin"   -< g
    sopacity    <- getAttrValue "stroke-opacity"    -< g
    fopacity    <- getAttrValue "fill-opacity"      -< g
    stro        <- getAttrValue "stroke"            -< g
    strowidth   <- getAttrValue "stroke-width"      -< g
    fll         <- getAttrValue "fill"              -< g
    slinecap    <- getAttrValue "stroke-linecap"    -< g
    smiterlimit <- getAttrValue "stroke-miterlimit" -< g
    gsvgClass   <- getAttrValue "class"             -< g
    gpaths      <- listA getPath                    -< g
    returnA -< SVGGroup
      { strokeLinejoin = T.pack sljoin,
        strokeOpacity  = T.pack sopacity,
        fillOpacity  = T.pack fopacity,
        stroke  = T.pack stro,
        strokeWidth  = T.pack strowidth,
        fill  = T.pack fll,
        strokeLinecap  = T.pack slinecap,
        strokeMiterlimit  = T.pack smiterlimit,
        svgClass = T.pack gsvgClass,
        paths = gpaths }

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
    sxmlns         <- getAttrValue "xmlns"          -< s
    sheight        <- getAttrValue "height"         -< s
    sstrokeOpacity <- getAttrValue "stroke-opacity" -< s
    sviewBox       <- getAttrValue "viewBox"        -< s
    sfontSize      <- getAttrValue "font-size"      -< s
    swidth         <- getAttrValue "width"          -< s
    sxmlnsXlink    <- getAttrValue "xmlns:xlink"    -< s
    sstroke        <- getAttrValue "stroke"         -< s
    sversion       <- getAttrValue "version"        -< s
    sgroups        <- listA getGroup -< s
    returnA -< SVGOptions
      {
          xmlns = T.pack sxmlns,
          height = T.pack sheight,
          iStrokeOpacity = T.pack sstrokeOpacity,
          viewBox = T.pack sviewBox,
          fontSize = T.pack sfontSize,
          width = T.pack swidth,
          xmlnsXlink = T.pack sxmlnsXlink,
          iStroke = T.pack sstroke,
          version = T.pack sversion,
          groups = sgroups
      }

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
formatSVG (x:xs) = x{ paths = gpaths } : formatSVG rest
                where
                  gpaths
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
  let s = w2c <$> LBS.unpack s'
  (x:_) <- runX (parseXML (removeDoctype False s) >>> getSVGAttributes)
  return $ XML.renderText XML.def $ XML.Document
    (XML.Prologue [] Nothing [])
    (buildSVG x{groups = formatSVG (groups x)})
    []
