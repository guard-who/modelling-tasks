{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Modelling.Auxiliary.Diagrams (
  arrowheadDiamond,
  arrowheadFilledDiamond,
  arrowheadTriangle,
  arrowheadV,
  arrowheadVee,
  connectOutside'',
  connectWithPath,
  flipArrow,
  nonEmptyPathBetween,
  renderSVG,
  text',
  textU,
  trailBetween,
  varrow,
  ) where

import Control.Lens.Operators           ((.~), (^.))
import Data.ByteString.Lazy             (ByteString)
import Data.Data                        (Typeable)
import Data.Function                    ((&))
import Data.GraphViz                    (DirType (Back, Both, Forward, NoDir))
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Semigroup                   (Any)
import Diagrams.Angle (
  Angle,
  (@@),
  cosA,
  deg,
  halfTurn,
  quarterTurn,
  rotate,
  sinA,
  tanA,
  )
import Diagrams.Attributes (
#if MIN_VERSION_SVGFonts(1,8,0)
  lw,
  none,
#endif
  lwL,
  )
import Diagrams.Backend.SVG (
  B,
  Options (SVGOptions),
  SVG (SVG),
  svgClass,
  )
import Diagrams.BoundingBox             (boundingBox, boxExtents)
import Diagrams.Combinators             (atop)
import Diagrams.Located                 (Located (unLoc), at)
import Diagrams.Names                   (IsName (toName), location, lookupName)
import Diagrams.Parametric              (Codomain, Parametric, atParam)
import Diagrams.Path                    (Path, pathPoints, pathTrails)
import Diagrams.Points                  (Point (P), (*.))
import Diagrams.Prelude (
  (*^),
  (^+^),
  (^-^),
  (^/),
  Affine (..),
#if MIN_VERSION_SVGFonts(1,8,0)
  Default (def),
#endif
  Diagram,
  Metric,
  N,
  OrderedField,
  QDiagram,
  V,
  V2,
  _x,
  _y,
  black,
  distanceA,
  fromMeasured,
  negated,
  norm,
  renderDia,
  unP,
  unitX,
  unitY,
  )
import Diagrams.Size                    (SizeSpec)
import Diagrams.Tangent (
  Tangent,
  normalAtParam,
  tangentAtEnd,
  tangentAtStart,
  )
import Diagrams.Trace                   (maxTraceP, traceP)
import Diagrams.Trail                   (Trail)
import Diagrams.TrailLike               (fromVertices)
import Diagrams.Transform               (place, scale, translate)
import Diagrams.TwoD.Arc                (arcCCW, arcCW)
import Diagrams.TwoD.Align              (alignL, alignR, centerXY)
import Diagrams.TwoD.Arrow              (
  ArrowOpts,
  arrowBetween',
  arrowHead,
  arrowShaft,
  arrowTail,
  headGap,
  headLength,
  tailGap,
  tailLength,
  )
import Diagrams.TwoD.Arrowheads         (ArrowHT)
import Diagrams.TwoD.Attributes         (fc, lc)
import Diagrams.TwoD.Polygons (
  PolyOrientation (NoOrient),
  PolyType (PolySides),
  polyOrient,
  polyType,
  polygon,
  )
import Diagrams.TwoD.Shapes             (rect)
import Diagrams.TwoD.Transform          (reflectX, scaleX, scaleY)
import Diagrams.TwoD.Vector             (angleDir, signedAngleBetween, unit_X)
import Diagrams.Util                    ((#), with)
import Graphics.SVGFonts (
  TextOpts (..),
#if MIN_VERSION_SVGFonts(1,8,0)
  fit_height,
  set_envelope,
  svgText,
#else
  Spacing (..),
  Mode (..),
  textSVG_,
#endif
  )
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Graphics.Svg.Core                (renderBS)

varrow :: ArrowHT Double
varrow = arrowheadVee (150 @@ deg)

arrowheadV :: RealFloat n => Angle n -> ArrowHT n
arrowheadV theta len shaftWidth =
  (jt # alignR, line # alignR)
  where
    shift right = translate (unP $ (factor * sinA theta * len / 2) *. unitY)
                . translate (unP $ (cosA theta * len / 2) *. unitX)
      where factor = if right then -1 else 1
    mtheta = - theta ^. deg @@ deg
    jt = shift True (rotate mtheta line) <> shift False (rotate theta line)
      -- <> translate (unP $ (shaftWidth * sinA theta / 2) *. unitX) tip
    -- tip = rotate (-90 @@ deg) (scaleY (sinA theta) (triangle shaftWidth))
    line = rect len shaftWidth

arrowheadVee :: RealFloat n => Angle n -> ArrowHT n
arrowheadVee theta len shaftWidth = (
  rotate (negated quarterTurn) (
    polygon $ with
      & polyType .~ PolySides
      [a1, quarterTurn, quarterTurn, a2, quarterTurn, quarterTurn, a1, quarterTurn, quarterTurn]
        [start, len', w, len, len, w, len', start, w]
      & polyOrient .~ NoOrient
    )
    # alignL,
  mempty
  )
  where
    start = len
    len' = len - w / tanA theta' - w / 2 / sinA theta'
    theta' = halfTurn ^-^ theta
    a1 = halfTurn ^+^ theta'
    a2 = theta ^-^ theta'
    w = shaftWidth

arrowheadTriangle :: RealFloat n => Angle n -> ArrowHT n
arrowheadTriangle theta len shaftWidth = (
  rotate (negated quarterTurn) (
    polygon $ with
      & polyType .~ PolySides
        [negated quarterTurn, a1, a2, a1, negated quarterTurn, quarterTurn, quarterTurn, quarterTurn, a1', a2', a1', quarterTurn, negated quarterTurn, quarterTurn]
        [start, len', len, len, len', start, w / 2, w + start, lenI', lenI, lenI, lenI', w + start, w / 2]
      & polyOrient .~ NoOrient
    )
    # alignL,
  mempty
  )
  where
    start = 0
    len' = len * sinA theta' - w / 2
    lenI' = lenI * sinA theta'
    lenI = len - w / cosA theta' - w / tanA theta' - w * tanA theta'
    theta' = halfTurn ^-^ theta
    a1 = quarterTurn ^+^ theta'
    a2 = theta ^-^ theta'
    a2' = negated a2
    a1' = negated a1
    w = shaftWidth

arrowheadDiamond :: RealFloat n => Angle n -> ArrowHT n
arrowheadDiamond theta len shaftWidth = (
  rotate (negated quarterTurn) (
    polygon $ with
      & polyType .~ PolySides
        [a3, a1, a2, a1, a3, quarterTurn, quarterTurn, a3', a1', a2', a1', a3', quarterTurn, quarterTurn]
        [w, len', len, len, len', w, w / 2, dw, lenI', lenI', lenI', lenI', dw, w / 2]
      & polyOrient .~ NoOrient
    )
    # alignL,
  mempty
  )
  where
    dw = w + w / sinA theta' / 2
    w' = shaftWidth / sinA theta / 2
    len' = len - w'
    lenI' = len - 2 * w / sinA a1
    theta' = halfTurn ^-^ theta
    a1 = 2 *^ theta'
    a2 = halfTurn ^-^ a1
    a3 = halfTurn ^+^ theta
    a3' = negated a3
    a2' = negated a2
    a1' = negated a1
    w = shaftWidth

arrowheadFilledDiamond :: RealFloat n => Angle n -> ArrowHT n
arrowheadFilledDiamond theta len shaftWidth = (
  reflectX $ (
    polygon $ with
      & polyType .~ PolySides
        [quarterTurn, a3, a1, a2, a1, a3, quarterTurn, quarterTurn]
        [w, w, len', len, len, len', w, w]
      & polyOrient .~ NoOrient
    )
    # alignR,
  mempty
  )
  where
    len' = len - shaftWidth / sinA theta / 2
    theta' = halfTurn ^-^ theta
    a1 = 2 *^ theta'
    a2 = halfTurn ^-^ a1
    a3 = halfTurn ^+^ theta
    w = shaftWidth

nonEmptyPathBetween
  :: (IsName p1, IsName p2, Metric v, RealFloat n, Semigroup m)
  => Path v n
  -> p1
  -> p2
  -> QDiagram b v n m
  -> Path v n
nonEmptyPathBetween p ls lt g =
  let (x, y, z) = fromJust $ pointsFromTo ls lt g
  in case pathPoints p of
    [] -> fromVertices [x, y, z]
    _  -> p

trailBetween
  :: (IsName n1, IsName n2, Semigroup m)
  => Path V2 Double
  -> n1
  -> n2
  -> QDiagram b V2 Double m
  -> Located (Trail V2 Double)
trailBetween path l1 l2 d =
  let x = head $ pathTrails path
      points = head $ pathPoints path
      oldPos = head points
      oldE = last points
  in maybe
       x
       (\(pos, _, e) -> scaleAndPositionTrail pos e oldPos oldE x)
       $ if toName l1 == toName l2 then Nothing else pointsFromTo l1 l2 d

scaleAndPositionTrail
  :: Point V2 Double
  -> Point V2 Double
  -> Point V2 Double
  -> Point V2 Double
  -> Located (Trail V2 Double)
  -> Located (Trail V2 Double)
scaleAndPositionTrail pos e oldPos oldE x = scale
  (distanceA e pos / distanceA oldPos oldE)
  (unLoc x)
  `at` pos

pointsFromTo
  :: (IsName n1, IsName n2, Metric v, RealFloat n, Semigroup m)
  => n1
  -> n2
  -> QDiagram b v n m
  -> Maybe (Point v n, Point v n, Point v n)
pointsFromTo n1 n2 g = do
  b1 <- lookupName n1 g
  b2 <- lookupName n2 g
  let v = location b2 .-. location b1
      midpoint = location b1 .+^ (v ^/ 2)
      s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
      e' = fromMaybe (location b2) $ traceP midpoint v b2
  return (s', midpoint, e')

connectOutside''
  ::(IsName n1, IsName n2, RealFloat n, Show n, Typeable n)
  => ArrowOpts n
  -> n1
  -> n2
  -> QDiagram SVG V2 n Any
  -> QDiagram SVG V2 n Any
connectOutside'' opts n1 n2 g =
  let (s', _, e') = fromJust $ pointsFromTo n1 n2 g
  in arrowBetween' opts s' e' # svgClass "edge"
     `atop` g

pointsFromToWithAngle
  :: (IsName n1, IsName n2, RealFloat n)
  => n1
  -> n2
  -> Angle n
  -> Angle n
  -> QDiagram b V2 n Any
  -> Maybe (Point V2 n, Point V2 n)
pointsFromToWithAngle n1 n2 a1 a2 g = do
  sub1 <- lookupName n1 g
  sub2 <- lookupName n2 g
  let os = location sub1
      oe = location sub2
      s = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
      e = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
  return (s, e)

connectPerim''
  :: (IsName n1, IsName n2, RealFloat n, Typeable n, Show n)
  => ArrowOpts n
  -> n1
  -> n2
  -> Angle n
  -> Angle n
  -> QDiagram SVG V2 n Any
  -> QDiagram SVG V2 n Any
connectPerim'' opts n1 n2 a1 a2 g =
  let (s', e') = fromJust $ pointsFromToWithAngle n1 n2 a1 a2 g
  in arrowBetween' opts s' e' # svgClass "edge"
     `atop` g

trailBetweenWithAngle
  :: (IsName n1, IsName n2)
  => ArrowOpts Double
  -> Path V2 Double
  -> n1
  -> n2
  -> Angle Double
  -> Angle Double
  -> QDiagram b V2 Double Any
  -> Located (Trail V2 Double)
trailBetweenWithAngle opts path l1 l2 a1 a2 d = maybe
  x
  (uncurry rescale)
  $ pointsFromToWithAngle l1 l2 a1 a2 d
  where
   x = head $ pathTrails path
   rescale pos e
      | toName l1 == toName l2
      = translate (unP out) $ scaleAndPositionTrail pos e oldPos oldE selfArc
      | otherwise
      = scaleAndPositionTrail pos e oldPos oldE x
    where
    points = head $ pathPoints path
    oldPos = head points
    oldE = last points
    arc = if a1 > a2 then arcCW else arcCCW
    a1' = a1 ^+^ quarterTurn
    a2' = a2 ^-^ quarterTurn
    unitSelfArc = arc (angleDir a1') (angleDir a2')
    selfArc = scaleY (portion _y) $ scaleX (portion _x) unitSelfArc
    between = e' ^-^ pos'
    portion f = norm between / 2 * (eSelfArc ^.f / 2)
    eSelfArc = P $ boxExtents $ boundingBox unitSelfArc
    pos' = pos ^+^ out
    out = rotate
      (a1 ^-^ halfTurn)
      $ fromMeasured 1.0 1.0 (opts ^. tailLength) *. unit_X
    e' = e ^+^ rotate
      (a2 ^-^ halfTurn)
      (fromMeasured 1.0 1.0 (opts ^. headLength) *. unit_X)

connectWithPath
  :: (IsName n1, IsName n2)
  => ArrowOpts Double
  -> PreparedFont Double
  -> DirType
  -> n1
  -> n2
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Path V2 Double
  -> QDiagram SVG V2 Double Any
  -> QDiagram SVG V2 Double Any
connectWithPath opts font dir l1 l2 ml fl tl path g =
  foldr addLabel (connectPerim'' opts' l1 l2 ang1 ang2 g) lpoints # svgClass "."
  where
    lpoints = [(Middle, ml), (Begin, fl), (End, tl)]
    opts' = amendOptsByDirection opts dir
      & arrowShaft .~ unLoc shaft
    addLabel (loc, ml')
      | Just l <- ml' =
          let txt = centerXY $ text' font 16 l
              shift = boxExtents (boundingBox txt) ^/2
              param = dirParam loc dir
              dist = case loc of
                Begin
                  | dir == Back || dir == Both -> 4
                  | otherwise -> 2
                Middle -> 0
                End
                  | dir == Forward || dir == Both -> 4
                  | otherwise -> 2
              p = pointAtShiftedBy shaft param shift dist
          in atop (place txt p # svgClass "elabel")
      | otherwise     = id
    shaft = trailBetweenWithAngle opts' path l1 l2 ang1 ang2 g
    (ang1, ang2) = inAndOutAngle path l1 l2 g

amendOptsByDirection
  :: (Floating n, Ord n)
  => ArrowOpts n
  -> DirType
  -> ArrowOpts n
amendOptsByDirection opts dir = opts & case dir of
  Back -> amendTail . amendHead
  Both -> amendTail
  Forward -> id
  NoDir -> amendHead
  where
    head' = flipArrow $ opts ^. arrowTail
    amendHead x = x
      & arrowHead .~ head'
      & headLength .~ opts ^. tailLength
      & headGap .~ opts ^. tailGap
    tail' = flipArrow $ opts ^. arrowHead
    amendTail x = x
      & arrowTail .~ tail'
      & tailLength .~ opts ^. headLength
      & tailGap .~ opts ^. headGap

inAndOutAngle
  :: (IsName n1, IsName n2, Semigroup m)
  => Path V2 Double
  -> n1
  -> n2
  -> QDiagram b V2 Double m
  -> (Angle Double, Angle Double)
inAndOutAngle path l1 l2 g = (ang1, ang2)
  where
    trail = trailBetween path l1 l2 g
    ang1 = signedAngleBetween (tangentAtStart trail) (-unit_X) ^+^ adjustAngle
    ang2 = signedAngleBetween (tangentAtEnd trail) unit_X ^-^ adjustAngle
    adjustAngle
      | toName l1 == toName l2 = 20 @@ deg
      | otherwise              = 0 @@ deg

{-|
Makes an arrow head an arrow tail and vice versa.
-}
flipArrow :: OrderedField n => ArrowHT n -> ArrowHT n
flipArrow hd = tl
  where
    tl sz shaftWidth = (t, j)
      where
        (t', j') = hd sz shaftWidth
        t = reflectX t'
        j = reflectX j'

data Where = Begin | Middle | End

dirParam :: Fractional p => Where -> DirType -> p
dirParam Begin dir
  | dir == Back    = 0.93
  | otherwise      = 0.07
dirParam End dir
  | dir == Back    = 0.07
  | otherwise      = 0.93
dirParam Middle dir
  | dir == Forward = 0.4
  | dir == Back    = 0.6
  | otherwise      = 0.5

pointAtShiftedBy
  :: (Floating (N p), Affine (Codomain p), Parametric p, Parametric (Tangent p),
      Diff (Codomain p) ~ V2, V p ~ V2)
  => p
  -> N p
  -> V2 (N p)
  -> V2 (N p)
  -> Codomain p (N p)
pointAtShiftedBy shaft param v additionalDistance =
  shaft `atParam` param .-^ n' .-^ additionalDistance * n
  where
    n = shaft `normalAtParam` param
    x = v ^. _x
    y = v ^. _y
    n' = scaleX (abs x) . scaleY (abs y) $ n

{-|
Render text as a diagram.
-}
renderText
  :: Bool
  -- ^ whether to underline
  -> PreparedFont Double
  -- ^ which font to use
  -> Double
  -- ^ font size
  -> String
  -- ^ what to write
  -> Diagram B
renderText u pfont s x = x
#if MIN_VERSION_SVGFonts(1,8,0)
  # svgText (def :: TextOpts Double) { textFont = pfont, underline = u }
  # fit_height s
  # set_envelope
  # lw none
#else
  # textSVG_ (TextOpts pfont INSIDE_H KERN u s s)
#endif
  # fc black
  # lc black
  # lwL 0.4

{-|
Render normal text as a diagram.
-}
text'
  :: PreparedFont Double
  -- ^ which font to use
  -> Double
  -- ^ font size
  -> String
  -- ^ what to write
  -> Diagram B
text' = renderText False

{-|
Render underlined text as a diagram.
-}
textU
  :: PreparedFont Double
  -- ^ which font to use
  -> Double
  -- ^ font size
  -> String
  -- ^ what to write
  -> Diagram B
textU = renderText True

renderSVG :: (Show n, Typeable n, RealFloat n, Monoid m)
  => SizeSpec V2 n
  -> QDiagram
     SVG V2 n m
  -> ByteString
renderSVG spec = renderBS . renderDia SVG opts
  where
    opts = SVGOptions spec Nothing mempty [] True
