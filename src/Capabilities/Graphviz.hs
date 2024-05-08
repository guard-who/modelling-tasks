{-# LANGUAGE Rank2Types #-}
-- | Defines a Monad context for calling graphviz.

module Capabilities.Graphviz (
  MonadGraphviz (errorWithoutGraphviz, layoutGraph, layoutGraph'),
  ) where

import Control.Monad.Output.Generic     (GenericReportT)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.GraphViz (
  AttributeEdge,
  AttributeNode,
  GraphvizCommand,
  GraphvizParams,
  )
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

instance MonadGraphviz m => MonadGraphviz (GenericReportT l o m)  where
  errorWithoutGraphviz = lift errorWithoutGraphviz
  layoutGraph command = lift . layoutGraph command
  layoutGraph' params command = lift . layoutGraph' params command
