module Modelling.Auxiliary.Diagrams (
  renderSVG,
  varrow,
  ) where

import Data.ByteString.Lazy             (ByteString)
import Data.Data                        (Typeable)
import Diagrams.Backend.SVG             (Options (SVGOptions), SVG (SVG))
import Diagrams.Prelude (
  (*.),
  (@@),
  (^.),
  Angle,
  ArrowHT,
  QDiagram,
  SizeSpec,
  V2,
  cosA,
  deg,
  rect,
  renderDia,
  rotate,
  sinA,
  translate,
  unP,
  unitX,
  unitY,
  )
import Graphics.Svg.Core                (renderBS)

varrow :: ArrowHT Double
varrow = arrowheadV (160 @@ deg)

arrowheadV :: RealFloat n => Angle n -> ArrowHT n
arrowheadV theta len shaftWidth = (jt, mempty)
  where
    shift right = translate (unP $ (factor * sinA theta * len / 2) *. unitY)
                . translate (unP $ (cosA theta * len / 2) *. unitX)
      where factor = if right then -1 else 1
    mtheta = - theta ^. deg @@ deg
    jt = shift True (rotate mtheta line) <> shift False (rotate theta line)
      -- <> translate (unP $ (shaftWidth * sinA theta / 2) *. unitX) tip
    -- tip = rotate (-90 @@ deg) (scaleY (sinA theta) (triangle shaftWidth))
    line = rect len (shaftWidth / 8)

renderSVG :: (Show n, Typeable n, RealFloat n, Monoid m)
  => SizeSpec V2 n
  -> QDiagram
     SVG V2 n m
  -> ByteString
renderSVG spec = renderBS . renderDia SVG opts
  where
    opts = SVGOptions spec Nothing mempty [] True
