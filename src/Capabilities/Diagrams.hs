-- | Defines a Monad context for rendering diagrams graphics to file.

module Capabilities.Diagrams (
  MonadDiagrams (lin, writeSvg),
  ) where

import Control.Monad.Trans.Class                  (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (GenericReportT)
import Data.Data                                  (Typeable)
import Diagrams.Backend.SVG                       (SVG)
import Diagrams.Prelude                           (QDiagram)
import Diagrams.TwoD                              (V2)
import Graphics.SVGFonts.ReadFont                 (PreparedFont)

class Monad m => MonadDiagrams m where
  lin :: (Read n, RealFloat n) => m (PreparedFont n)
  writeSvg
    :: (Show n, Typeable n, RealFloat n, Monoid o)
    => FilePath
    -> QDiagram SVG V2 n o
    -> m ()

instance MonadDiagrams m => MonadDiagrams (GenericReportT l o m)  where
  lin = lift lin
  writeSvg file = lift . writeSvg file
