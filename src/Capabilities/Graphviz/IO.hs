{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Graphviz.

module Capabilities.Graphviz.IO () where

import qualified Diagrams.TwoD.GraphViz           as GV (
  layoutGraph,
  layoutGraph',
  )

import Capabilities.Graphviz            (MonadGraphviz (..))

import Data.GraphViz                    (quitWithoutGraphviz)
import Data.String.Interpolate          (iii)

instance MonadGraphviz IO where
  errorWithoutGraphviz =
    quitWithoutGraphviz [iii|
      Please install GraphViz executables from http://graphviz.org/
      and put them on your PATH
      |]
  layoutGraph = GV.layoutGraph
  layoutGraph' = GV.layoutGraph'
