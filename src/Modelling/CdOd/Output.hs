{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Modelling.CdOd.Output (
  cacheCd,
  cacheOd,
  drawCd,
  drawOdFromInstance,
  drawOd,
  ) where

import qualified Data.ByteString.Lazy.UTF8        as LBS (fromString)
import qualified Data.Bimap                       as BM (fromList, lookupR)
import qualified Data.Map                         as M (
  foldrWithKey,
  )
import qualified Diagrams.TwoD.GraphViz           as GV (getGraph)

import Capabilities.Cache               (MonadCache, cache, short)
import Capabilities.Diagrams            (MonadDiagrams (lin, writeSvg))
import Capabilities.Graphviz (
  MonadGraphviz (errorWithoutGraphviz, layoutGraph'),
  )
import Modelling.Auxiliary.Diagrams (
  arrowheadDiamond,
  arrowheadFilledDiamond,
  arrowheadTriangle,
  arrowheadVee,
  connectWithPath,
  flipArrow,
  text',
  textU,
  veeArrow,
  )
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  emptyArr,
  underlinedLabel,
  )
import Modelling.CdOd.Types (
  AnyCd,
  AnyClassDiagram (..),
  AnyRelationship,
  CdDrawSettings (..),
  InvalidRelationship (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectDiagram (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  calculateThickAnyRelationships, anonymiseObjects,
  )

import Control.Lens                     ((.~))
import Control.Monad                    (guard)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random (
  RandT,
  RandomGen,
  )
import Control.Monad.Trans              (MonadTrans(lift))
import Data.Bifunctor                   (Bifunctor (second))
import Data.Digest.Pure.SHA             (sha1, showDigest)
import Data.Graph.Inductive             (Gr, mkGraph)
import Data.GraphViz (
  DirType (Back, Forward, NoDir),
  GraphvizParams (..),
  Shape (BoxShape),
  arrowFrom,
  arrowTo,
  diamond,
  dirCommand,
  edgeEnds,
  noArrow,
  nonClusteredParams,
  oDiamond,
  shape,
  toLabel,
  toLabelValue,
  undirCommand,
  vee,
  )
import Data.GraphViz.Attributes.Complete (Attribute (..), DPoint (..), Label)
import Data.Function                    ((&))
import Data.List                        (elemIndex)
import Data.Maybe                       (fromJust, fromMaybe, maybeToList)
import Data.Ratio                       ((%))
import Data.Tuple.Extra                 (both)
import Diagrams.Align                   (center)
import Diagrams.Angle                   ((@@), cosA, deg, halfTurn)
import Diagrams.Attributes              (lineWidth, lwL)
import Diagrams.Backend.SVG             (B, svgClass)
import Diagrams.Combinators             (atop, frame)
import Diagrams.Names                   (IsName, named)
import Diagrams.Path                    (Path, reversePath)
import Diagrams.Points                  (Point(..))
import Diagrams.Prelude (
  Diagram,
  Style,
  (^-^),
  applyStyle,
  black,
  local,
  white,
  )
import Diagrams.Transform               (translate)
import Diagrams.TwoD                    (V2, bg, snugCenterXY)
import Diagrams.TwoD.Arrow (
  arrowHead,
  arrowTail,
  headGap,
  headLength,
  tailLength,
  )
import Diagrams.TwoD.Arrowheads         (lineTail)
import Diagrams.TwoD.Attributes         (fc, lc)
import Diagrams.Util                    ((#), with)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call              (AlloyInstance)

relationshipArrow
  :: CdDrawSettings
  -> Maybe Attribute
  -> Bool
  -> AnyRelationship String String
  -> [Attribute]
relationshipArrow CdDrawSettings {..} marking isThick =
  either getInvalidArrow getArrow
  where
    getInvalidArrow = \case
      InvalidInheritance {..} -> [
        arrowTo emptyArr,
        TailLabel $ multiplicity Nothing $ limits invalidSubClass,
        HeadLabel $ multiplicity Nothing $ limits invalidSuperClass
        ]
    getArrow = \case
      Inheritance {} -> [arrowTo emptyArr]
      Composition {..} -> [
          arrowFrom diamond,
          edgeEnds Back,
          TailLabel $ multiplicity
            (compositionWholeOmittedDefaultMultiplicity omittedDefaults)
            $ limits compositionWhole,
          HeadLabel $ multiplicity
            (associationOmittedDefaultMultiplicity omittedDefaults)
            $ limits compositionPart
          ]
        ++ concat [maybeToList marking | isThick]
        ++ [toLabel compositionName | printNames]
      Aggregation {..} -> [
          arrowFrom oDiamond,
          edgeEnds Back,
          TailLabel $ multiplicity
            (aggregationWholeOmittedDefaultMultiplicity omittedDefaults)
            $ limits aggregationWhole,
          HeadLabel $ multiplicity
            (associationOmittedDefaultMultiplicity omittedDefaults)
            $ limits aggregationPart
          ]
        ++ concat [maybeToList marking | isThick]
        ++ [toLabel aggregationName | printNames]
      Association {..} -> associationArrow ++ [
          TailLabel $ multiplicity
            (associationOmittedDefaultMultiplicity omittedDefaults)
            $ limits associationFrom,
          HeadLabel $ multiplicity
            (associationOmittedDefaultMultiplicity omittedDefaults)
            $ limits associationTo
          ]
        ++ concat [maybeToList marking | isThick]
        ++ [toLabel associationName | printNames]
    associationArrow
      | printNavigations = [arrowTo vee, ArrowSize 0.4]
      | otherwise        = [ArrowHead noArrow]

multiplicity :: Maybe (Int, Maybe Int) -> (Int, Maybe Int) -> Label
multiplicity def = toLabelValue . fromMaybe "" . rangeWithDefault def

cacheCd
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m)
  => CdDrawSettings
  -> Style V2 Double
  -> AnyCd
  -> FilePath
  -> m FilePath
cacheCd config@CdDrawSettings{..} marking syntax path =
  cache path ext "cd" syntax $ flip $
    drawCd config marking
  where
    ext = short printNavigations
      ++ short printNames
      ++ showDigest (sha1 . LBS.fromString $ show marking)
      ++ ".svg"

drawCd
  :: (MonadDiagrams m, MonadGraphviz m)
  => CdDrawSettings
  -> Style V2 Double
  -> AnyCd
  -> FilePath
  -> m FilePath
drawCd config marking cd@AnyClassDiagram {..} file = do
  let theNodes = anyClassNames
  let toIndexed xs = [(
          fromJust (elemIndex from theNodes),
          fromJust (elemIndex to theNodes),
          x
          )
        | x@(_, r) <- xs
        , let (from, to) = either getFromToInvalid getFromTo r
        ]
  let thickenedRelationships = toIndexed $ calculateThickAnyRelationships cd
  let graph = mkGraph (zip [0..] theNodes) thickenedRelationships
        :: Gr String (Bool, AnyRelationship String String)
  let params = nonClusteredParams {
        fmtNode = \(_,l) -> [
          toLabel l,
          shape BoxShape,
          Margin $ DVal 0.02,
          Width 0,
          Height 0,
          FontSize 16
          ],
        fmtEdge = \(_,_,(isThick, r)) -> FontSize 16
          : relationshipArrow config Nothing isThick r
        }
  errorWithoutGraphviz
  graph' <- layoutGraph' params dirCommand graph
  font <- lin
  let (nodes, edges) = GV.getGraph graph'
      graphNodes = M.foldrWithKey
        (\l p g -> drawClass font l p `atop` g)
        mempty
        nodes
      graphEdges = foldr
        (\(s, t, (isThick, r), p) g -> g # drawEdge font s t isThick r p)
        graphNodes
        edges
  writeSvg file graphEdges
  return file
  where
    getFromToInvalid = \case
      InvalidInheritance {..} -> both linking (invalidSubClass, invalidSuperClass)
    getFromTo x = case x of
      Inheritance {..} -> (subClass, superClass)
      Association {..} -> both linking (associationFrom, associationTo)
      Aggregation {..} -> both linking (aggregationWhole, aggregationPart)
      Composition {..} -> both linking (compositionWhole, compositionPart)
    drawEdge f = drawRelationship f config marking

drawRelationship
  :: IsName n
  => PreparedFont Double
  -> CdDrawSettings
  -> Style V2 Double
  -> n
  -> n
  -> Bool
  -> AnyRelationship n String
  -> Path V2 Double
  -> Diagram B
  -> Diagram B
drawRelationship font CdDrawSettings {..} marking fl tl isThick relationship path =
  connectWithPath opts font dir from to ml fromLimits toLimits path'
  # applyStyle (if isThick then marking else mempty)
  # lwL 0.5
  where
    angle = 150 @@ deg
    opts = with
      & arrowTail .~ theTail angle
      & arrowHead .~ theHead angle
      & headLength .~ local headSize
      & headGap .~ local 0
      & tailLength .~ local tailSize
    startLimits = case relationship of
      Left InvalidInheritance {..} ->
        rangeWithDefault Nothing $ limits invalidSubClass
      Right Inheritance {} -> Nothing
      Right Composition {..} ->
        rangeWithDefault
          (compositionWholeOmittedDefaultMultiplicity omittedDefaults)
          $ limits compositionWhole
      Right Aggregation {..} ->
        rangeWithDefault
          (aggregationWholeOmittedDefaultMultiplicity omittedDefaults)
          $ limits aggregationWhole
      Right Association {..} ->
        rangeWithDefault
          (associationOmittedDefaultMultiplicity omittedDefaults)
          $ limits associationFrom
    endLimits = case relationship of
      Left InvalidInheritance {..} ->
        rangeWithDefault Nothing $ limits invalidSuperClass
      Right Inheritance {} -> Nothing
      Right Composition {..} ->
        rangeWithDefault
          (associationOmittedDefaultMultiplicity omittedDefaults)
          $ limits compositionPart
      Right Aggregation {..} ->
        rangeWithDefault
          (associationOmittedDefaultMultiplicity omittedDefaults)
          $ limits aggregationPart
      Right Association {..} ->
        rangeWithDefault
          (associationOmittedDefaultMultiplicity omittedDefaults)
          $ limits associationTo
    (from, to, fromLimits, toLimits, path')
      | flipEdge  = (tl, fl, endLimits, startLimits, reversePath path)
      | otherwise = (fl, tl, startLimits, endLimits, path)
    (tailSize, headSize) = (7 * tailScaleFactor, 7)
    theTail = const lineTail
    triangleFactor = cosA (halfTurn ^-^ angle)
    diamondFactor = 2 * cosA (halfTurn ^-^ angle)
    (flipEdge, theHead, tailScaleFactor) = case relationship of
      Left InvalidInheritance {} -> (False, arrowheadTriangle, 0.9)
      Right Inheritance {} -> (False, arrowheadTriangle, 0.9)
      Right Association {} -> (
          False,
          if printNavigations then arrowheadVee else const (flipArrow lineTail),
          triangleFactor
          )
      Right (Aggregation {}) -> (True, arrowheadDiamond, diamondFactor)
      Right (Composition {}) -> (True, arrowheadFilledDiamond, diamondFactor)
    dir = case relationship of
      Left InvalidInheritance {} -> Forward
      Right Association {} ->
        if printNavigations then Forward else NoDir
      Right Aggregation {} -> Forward
      Right Composition {} -> Forward
      Right Inheritance {} -> Forward
    ml = do
      guard printNames
      case relationship of
        Left InvalidInheritance {} -> Nothing
        Right Inheritance {} -> Nothing
        Right Association {..} -> Just associationName
        Right Aggregation {..} -> Just aggregationName
        Right Composition {..} -> Just compositionName

rangeWithDefault :: Maybe (Int, Maybe Int) -> (Int, Maybe Int) -> Maybe String
rangeWithDefault def fromTo
  | def == Just fromTo = Nothing
  | otherwise     = Just $ range fromTo
  where
    range (l, Nothing) = show l ++ "..*"
    range (l, Just u)
      | l == -1   = "*.." ++ show u
      | l == u    = show l
      | otherwise = show l ++ ".." ++ show u

drawClass
  :: PreparedFont Double
  -> String
  -> Point V2 Double
  -> Diagram B
drawClass font l (P p) = translate p
  $ center $ blackFrame l $ center
  $ text' font 16 l
  # snugCenterXY
  # lineWidth 0.6
  # svgClass "label"

drawOdFromInstance
  :: (MonadDiagrams m, MonadGraphviz m, MonadThrow m, RandomGen g)
  => AlloyInstance
  -> Maybe Rational
  -> DirType
  -> Bool
  -> FilePath
  -> RandT g m FilePath
drawOdFromInstance i anonymous direction printNames path = do
  g <- alloyInstanceToOd i
  od <- anonymiseObjects (fromMaybe (1 % 3) anonymous) g
  lift $ drawOd
    od
    direction
    printNames
    path

cacheOd
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m)
  => Od
  -> DirType
  -> Bool
  -> FilePath
  -> m FilePath
cacheOd od direction printNames path =
  cache path ext "od" od $ \file od' ->
    drawOd od' direction printNames file
  where
    ext = short printNames
      ++ short direction
      ++ ".svg"

drawOd
  :: (MonadDiagrams m, MonadGraphviz m, MonadThrow m)
  => Od
  -> DirType
  -> Bool
  -> FilePath
  -> m FilePath
drawOd ObjectDiagram {..} direction printNames file = do
  let numberedObjects = zip [0..] objects
      bmObjects = BM.fromList $ map (second objectName) numberedObjects
      toEdge l@Link {..} = (,,)
        <$> BM.lookupR linkFrom bmObjects
        <*> BM.lookupR linkTo bmObjects
        <*> pure l
  linkEdges <- mapM toEdge links
  let graph = mkGraph numberedObjects linkEdges
  let objectNames = map (\x -> (objectName x, objectName x ++ " "))
        $ filter (not . isAnonymous) objects
  let params = nonClusteredParams {
        fmtNode = \(_, Object {..}) -> [
          underlinedLabel $ fromMaybe "" (lookup objectName objectNames)
          ++ ": " ++ objectClass,
          shape BoxShape,
          Margin $ DVal 0.02,
          Width 0,
          Height 0,
          FontSize 16
          ],
        fmtEdge = \(_,_,Link {..}) -> arrowHeads
          ++ [ArrowSize 0.4, FontSize 16]
          ++ [toLabel linkName | printNames] }
  errorWithoutGraphviz
  graph' <- layoutGraph' params undirCommand graph
  font <- lin
  let (nodes, edges) = GV.getGraph graph'
      graphNodes = M.foldrWithKey
        (\l p g -> drawObject font objectNames l p `atop` g)
        mempty
        nodes
      graphEdges = foldr
        (\(Object {objectName = s}, Object {objectName = t}, l, p) g ->
           g # drawLink font direction printNames s t l p)
        graphNodes
        edges
  writeSvg file graphEdges
  return file
  where
    arrowHeads = case direction of
      NoDir  -> [edgeEnds NoDir]
      dir -> [edgeEnds dir, arrowFrom vee, arrowTo vee]

drawLink
  :: (IsName n1, IsName n2)
  => PreparedFont Double
  -> DirType
  -> Bool
  -> n1
  -> n2
  -> Link String String
  -> Path V2 Double
  -> Diagram B
  -> Diagram B
drawLink font direction printNames fl tl Link {..} =
  connectWithPath opts font direction fl tl ml Nothing Nothing
  # lwL 0.5
  where
    opts = with
      & arrowTail .~ lineTail
      & arrowHead .~ veeArrow
      & headLength .~ local 7
      & headGap .~ local 0
      & tailLength .~ local 7
    ml
      | printNames = Just linkName
      | otherwise  = Nothing

drawObject
  :: PreparedFont Double
  -> [(String, String)]
  -> Object String String
  -> Point V2 Double
  -> Diagram B
drawObject font objectNames Object {..} (P p) = translate p
  $ center $ blackFrame objectName $ center
  $ textU font 16
      (fromMaybe "" (lookup objectName objectNames) ++ ": " ++ objectClass)
  # snugCenterXY
  # lineWidth 0.6
  # svgClass "label"

blackFrame
  :: String
  -> Diagram B
  -> Diagram B
blackFrame t object =
  frame 1 (
    frame 2 object
      # fc black
      # lc black
      # bg white
      # svgClass "bg"
    )
  # bg black
  # named t
  # svgClass "node"
