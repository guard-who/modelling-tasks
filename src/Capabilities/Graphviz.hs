{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- | Defines a Monad context for calling graphviz.

module Capabilities.Graphviz (
  MonadGraphviz (errorWithoutGraphviz, layoutGraph, layoutGraph'),
  ) where

import qualified Diagrams.TwoD.GraphViz           as GV

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output.Generic     (GenericReportT)
import Data.GraphViz (
  AttributeEdge,
  AttributeNode,
  GraphvizCommand,
  GraphvizParams,
  quitWithoutGraphviz,
  )
import Data.String.Interpolate          (iii)
import Data.Graph.Inductive.Graph       (Graph, Node)

class Monad m => MonadGraphviz m where
  errorWithoutGraphviz :: m ()
  layoutGraph
    :: forall gr v e . Graph gr
    => GraphvizCommand
    -> gr v e
    -> m (gr (AttributeNode v) (AttributeEdge e))
  layoutGraph'
    :: (Ord cl, Graph gr)
    => GraphvizParams Node v e cl l
    -> GraphvizCommand
    -> gr v e
    -> m (gr (AttributeNode v) (AttributeEdge e))

instance MonadGraphviz IO where
  errorWithoutGraphviz =
    quitWithoutGraphviz [iii|
      Please install GraphViz executables from http://graphviz.org/
      and put them on your PATH
      |]
  layoutGraph = GV.layoutGraph
  layoutGraph' = GV.layoutGraph'

instance MonadIO m => MonadGraphviz (GenericReportT l o m)  where
  errorWithoutGraphviz = liftIO errorWithoutGraphviz
  layoutGraph command = liftIO . layoutGraph command
  layoutGraph' params command = liftIO . layoutGraph' params command
