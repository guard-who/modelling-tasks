{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

import Capabilities.Cache               (MonadCache, cache, cacheT, short)
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
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectDiagram (..),
  Od,
  Relationship (..),
  calculateThickRelationships,
  )

import Control.Lens                     ((.~))
import Control.Monad                    (guard)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random (
  MonadRandom (getRandom),
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
import Data.Tuple.Extra                 (both)
import Diagrams.Align                   (center)
import Diagrams.Angle                   ((@@), deg)
import Diagrams.Attributes              (lineWidth, lwL)
import Diagrams.Backend.SVG             (B, svgClass)
import Diagrams.Combinators             (atop, frame)
import Diagrams.Names                   (IsName, named)
import Diagrams.Path                    (Path, reversePath)
import Diagrams.Points                  (Point(..))
import Diagrams.Prelude (
  Diagram,
  Style,
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
import System.Random.Shuffle            (shuffleM)

relationshipArrow
  :: Bool
  -> Bool
  -> Maybe Attribute
  -> Bool
  -> Relationship String String
  -> [Attribute]
relationshipArrow printNavigations printNames marking isThick relationship =
  case relationship of
    Inheritance {} -> [arrowTo emptyArr]
    Composition {..} -> [
        arrowFrom diamond,
        edgeEnds Back,
        HeadLabel $ mult $ limits compositionPart
        ]
      ++ concat [maybeToList marking | isThick]
      ++ [toLabel compositionName | printNames]
      ++ case limits compositionWhole of
           (1, Just 1)   -> []
           l@(0, Just 1) -> [TailLabel $ mult l]
           (0, Nothing)  -> [TailLabel $ toLabelValue "0..*"]
           l@(_, _)      -> [TailLabel $ mult l]
    Aggregation {..} -> [
        arrowFrom oDiamond,
        edgeEnds Back,
        TailLabel $ mult $ limits aggregationWhole,
        HeadLabel $ mult $ limits aggregationPart
        ]
      ++ concat [maybeToList marking | isThick]
      ++ [toLabel aggregationName | printNames]
    Association {..} -> associationArrow ++ [
        TailLabel $ mult $ limits associationFrom,
        HeadLabel $ mult $ limits associationTo
        ]
      ++ concat [maybeToList marking | isThick]
      ++ [toLabel associationName | printNames]
  where
    associationArrow
      | printNavigations = [arrowTo vee, ArrowSize 0.4]
      | otherwise        = [ArrowHead noArrow]

mult :: (Int, Maybe Int) -> Label
mult (-1, Just u) = toLabelValue $ "*.." ++ show u
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

cacheCd
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m)
  => Bool
  -> Bool
  -> Style V2 Double
  -> Cd
  -> FilePath
  -> m FilePath
cacheCd printNavigations printNames marking syntax path =
  cache path ext "cd" syntax $ flip $
    drawCd printNavigations printNames marking
  where
    ext = short printNavigations
      ++ short printNames
      ++ showDigest (sha1 . LBS.fromString $ show marking)
      ++ ".svg"

drawCd
  :: (MonadDiagrams m, MonadGraphviz m)
  => Bool
  -> Bool
  -> Style V2 Double
  -> Cd
  -> FilePath
  -> m FilePath
drawCd printNavigations printNames marking cd@ClassDiagram {..} file = do
  let theNodes = classNames
  let toIndexed xs = [(
          fromJust (elemIndex from theNodes),
          fromJust (elemIndex to theNodes),
          x
          )
        | x@(_, r) <- xs
        , let (from, to) = getFromTo r
        ]
  let thickenedRelationships = toIndexed $ calculateThickRelationships cd
  let graph = mkGraph (zip [0..] theNodes) thickenedRelationships
        :: Gr String (Bool, Relationship String String)
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
          : relationshipArrow printNavigations printNames Nothing isThick r
        }
  errorWithoutGraphviz
  graph' <- layoutGraph' params dirCommand graph
  sfont  <- lin
  let (nodes, edges) = GV.getGraph graph'
      gnodes = M.foldrWithKey
        (\l p g -> drawClass sfont l p `atop` g)
        mempty
        nodes
      gedges = foldr
        (\(s, t, (isThick, r), p) g -> g # drawRel sfont s t isThick r p)
        gnodes
        edges
  writeSvg file gedges
  return file
  where
    getFromTo x = case x of
      Inheritance {..} -> (subClass, superClass)
      Association {..} -> both linking (associationFrom, associationTo)
      Aggregation {..} -> both linking (aggregationWhole, aggregationPart)
      Composition {..} -> both linking (compositionWhole, compositionPart)
    drawRel f = drawRelationship f printNavigations printNames marking

drawRelationship
  :: IsName n
  => PreparedFont Double
  -> Bool
  -> Bool
  -> Style V2 Double
  -> n
  -> n
  -> Bool
  -> Relationship n String
  -> Path V2 Double
  -> Diagram B
  -> Diagram B
drawRelationship sfont printNavigations printNames marking fl tl isThick l path =
  connectWithPath opts sfont dir from to ml mfl mtl path'
  # applyStyle (if isThick then marking else mempty)
  # lwL 0.5
  where
    angle :: Double
    angle = 150
    opts = with
      & arrowTail .~ atail (angle @@ deg)
      & arrowHead .~ ahead (angle @@ deg)
      & headLength .~ local 7
      & headGap .~ local 0
      & tailLength .~ local 7
    mfl' = case l of
      Inheritance {} -> Nothing
      Composition {..} ->
        rangeWithDefault (1, Just 1) $ limits compositionWhole
      Aggregation {..} ->
        rangeWithDefault (0, Nothing) $ limits aggregationWhole
      Association {..} ->
        rangeWithDefault (0, Nothing) $ limits associationFrom
    mtl' = case  l of
      Inheritance {} -> Nothing
      Composition {..} ->
        rangeWithDefault (0, Nothing) $ limits compositionPart
      Aggregation {..} ->
        rangeWithDefault (0, Nothing) $ limits aggregationPart
      Association {..} ->
        rangeWithDefault (0, Nothing) $ limits associationTo
    (from, to, mfl, mtl, path')
      | flipEdge  = (tl, fl, mtl', mfl', reversePath path)
      | otherwise = (fl, tl, mfl', mtl', path)
    atail = const lineTail
    (flipEdge, ahead) = case l of
      Inheritance {} -> (False, arrowheadTriangle)
      Association {} -> (
          False,
          if printNavigations then arrowheadVee else const (flipArrow lineTail)
          )
      Aggregation {} -> (True, arrowheadDiamond)
      Composition {} -> (True, arrowheadFilledDiamond)
    dir = case l of
      Association {} ->
        if printNavigations then Forward else NoDir
      Aggregation {} -> Forward
      Composition {} -> Forward
      Inheritance {} -> Forward
    ml = do
      guard printNames
      case l of
        Inheritance {}   -> Nothing
        Association {..} -> Just associationName
        Aggregation {..} -> Just aggregationName
        Composition {..} -> Just compositionName

rangeWithDefault :: (Int, Maybe Int) -> (Int, Maybe Int) -> Maybe String
rangeWithDefault def fromTo
  | def == fromTo = Nothing
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
drawClass sfont l (P p) = translate p
  $ center $ blackFrame l $ center
  $ text' sfont 16 l
  # snugCenterXY
  # lineWidth 0.6
  # svgClass "label"

drawOdFromInstance
  :: (MonadDiagrams m, MonadGraphviz m, MonadThrow m, RandomGen g)
  => AlloyInstance
  -> Maybe Int
  -> DirType
  -> Bool
  -> FilePath
  -> RandT g m FilePath
drawOdFromInstance i anonymous direction printNames path = do
  g <- alloyInstanceToOd i
  drawOd
    g
    (fromMaybe (length (objects g) `div` 3) anonymous)
    direction
    printNames
    path

cacheOd
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, RandomGen g)
  => Od
  -> Int
  -> DirType
  -> Bool
  -> FilePath
  -> RandT g m FilePath
cacheOd od anonymous direction printNames path = do
  x <- getRandom
  cacheT path (ext x) "od" od $ \file od' ->
    drawOd od' anonymous direction printNames file
  where
    ext x = short anonymous
      ++ short printNames
      ++ short direction
      ++ show @Int x
      ++ ".svg"

drawOd
  :: (MonadDiagrams m, MonadGraphviz m, MonadThrow m, RandomGen g)
  => Od
  -> Int
  -> DirType
  -> Bool
  -> FilePath
  -> RandT g m FilePath
drawOd ObjectDiagram {..} anonymous direction printNames file = do
  let numberedObjects = zip [0..] objects
      bmObjects = BM.fromList $ map (second objectName) numberedObjects
      toEdge l@Link {..} = (,,)
        <$> BM.lookupR linkFrom bmObjects
        <*> BM.lookupR linkTo bmObjects
        <*> pure l
  linkEdges <- lift $ mapM toEdge links
  let graph = mkGraph numberedObjects linkEdges
  objectNames <-
    map (\x -> (objectName x, objectName x ++ " "))
    . drop anonymous
    <$> shuffleM objects
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
  lift errorWithoutGraphviz
  graph' <- lift $ layoutGraph' params undirCommand graph
  sfont  <- lift lin
  let (nodes, edges) = GV.getGraph graph'
      gnodes = M.foldrWithKey
        (\l p g -> drawObject sfont objectNames l p `atop` g)
        mempty
        nodes
      gedges = foldr
        (\(Object {objectName = s}, Object {objectName = t}, l, p) g ->
           g # drawLink sfont direction printNames s t l p)
        gnodes
        edges
  lift $ writeSvg file gedges
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
drawLink sfont direction printNames fl tl Link {..} =
  connectWithPath opts sfont direction fl tl ml Nothing Nothing
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
drawObject sfont objectNames Object {..} (P p) = translate p
  $ center $ blackFrame objectName $ center
  $ textU sfont 16
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
