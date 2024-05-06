{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Provides the ability to render Petri nets.
-}
module Modelling.PetriNet.Diagram (
  cacheNet,
  drawNet,
  getDefaultNet,
  getNet,
  renderWith,
  ) where

import qualified Diagrams.TwoD.GraphViz           as GV (getGraph)
import qualified Data.Map                         as M (foldlWithKey)

import Capabilities.Cache               (MonadCache, cache, short)
import Capabilities.Diagrams            (MonadDiagrams (lin, writeSvg))
import Capabilities.Graphviz            (MonadGraphviz (layoutGraph))
import Modelling.Auxiliary.Common       (Object)
import Modelling.Auxiliary.Diagrams (
  connectOutside'',
  nonEmptyPathBetween,
  text',
  trailBetween,
  )
import Modelling.PetriNet.Parser (
  netToGr,
  parseNet,
  simpleRenameWith,
  )
import Modelling.PetriNet.Types (
  DrawSettings (..),
  Net (mapNet, traverseNet),
  )

import Control.Arrow                    (first)
import Control.Monad.Catch              (MonadThrow (throwM), Exception)
import Data.Graph.Inductive             (Gr)
import Data.GraphViz                    hiding (Path)
import Data.List                        (foldl')
import Diagrams.Backend.SVG             (B, svgClass)
import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call              (AlloyInstance)

cacheNet
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => String
  -> (a -> String)
  -> p n a
  -> Bool
  -> Bool
  -> Bool
  -> GraphvizCommand
  -> m FilePath
cacheNet path labelOf pl hidePNames hideTNames hide1 gc =
  cache path ext "petri" (mapNet labelOf pl) $ \svg pl' -> do
    dia <- drawNet id pl' hidePNames hideTNames hide1 gc
    writeSvg svg dia
  where
    ext = short hidePNames
      ++ short hideTNames
      ++ short hide1
      ++ short gc
      ++ ".svg"

newtype UnknownPetriNetNodeException
  = CouldNotFindNodeWithinGraph String
  deriving Show

instance Exception UnknownPetriNetNodeException

{-| Create a 'Diagram's graph of a Petri net like graph definition ('Net')
by distributing places and transitions using GraphViz.
The provided 'GraphvizCommand' is used for this distribution.
-}
drawNet
  :: (MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n, Ord a)
  => (a -> String)
  -- ^ how to obtain labels of the nodes
  -> p n a
  -- ^ the graph definition
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> GraphvizCommand
  -- ^ how to distribute the nodes
  -> m (Diagram B)
drawNet labelOf pl hidePNames hideTNames hide1 gc = do
  gr    <- either (throwM . CouldNotFindNodeWithinGraph . labelOf) return
    $ netToGr pl
  graph <- layoutGraph gc gr
  pfont <- lin
  return $ drawGraph labelOf hidePNames hideTNames hide1 pfont graph

getNet
  :: (MonadThrow m, Net p n, Traversable t)
  => (AlloyInstance -> m (t Object))
  -> AlloyInstance
  -> m (p n String, t String)
getNet parseInst inst = do
  (net, rename) <-
    getNetWith "flow" "tokens" inst
  conc <- parseInst inst
  rconc <- traverse rename conc
  return (net, rconc)

getDefaultNet
  :: (MonadThrow m, Net p n)
  => AlloyInstance
  -> m (p n String)
getDefaultNet inst= fst <$>
  getNetWith "defaultFlow" "defaultTokens" inst

{-|
Returns a Petri net like graph using 'parseNet'.
It additionally parses another part of the instance.
All nodes are renamed using the 'simpleRenameWith' function.
The renaming is also applied to the additionally parsed instance.
-}
getNetWith
  :: (MonadThrow m, Net p n)
  => String
  -- ^ flow
  -> String
  -- ^ tokens
  -> AlloyInstance
  -- ^ the instance to parse
  -> m (p n String, Object -> m String)
getNetWith f t inst = do
  pl <- parseNet f t inst
  let rename = simpleRenameWith pl
  pl' <- traverseNet rename pl
  return (pl', rename)

{-|
Obtain the Petri net like graph by drawing Nodes and connections between them
using the specific functions @drawNode@ and @drawEdge@.
-}
drawGraph
  :: Ord a
  => (a -> String)
  -- ^ how to obtain labels from nodes
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> PreparedFont Double
  -- ^ the font to be used for labels
  -> Gr (AttributeNode (a, Maybe Int)) (AttributeEdge Int)
  -- ^ the graph consisting of nodes and edges
  -> Diagram B
drawGraph labelOf hidePNames hideTNames hide1 pfont graph = gedges # frame 1
  where
    (nodes, edges) = GV.getGraph graph
    gnodes = M.foldlWithKey
      (\g l p -> g `atop` drawNode hidePNames hideTNames pfont (withLabel l) p)
      mempty
      nodes
    gedges = foldl
      (\g (s, t, l, p) ->
        let ls = labelOnly s
            lt = labelOnly t
        in g # drawEdge hide1 pfont l ls lt (nonEmptyPathBetween p ls lt g)
      )
      gnodes
      edges
    withLabel = first labelOf
    labelOnly = labelOf . fst

{-|
Nodes are either Places (having 'Just' tokens), or Transitions (having
'Nothing').
Transitions are drawn as squares.
Places are drawn as circles.
Places contain circled tokens layout as a ring of tokens or numbered
if 5 or more tokens are within.
Each node gets a label.
-}
drawNode
  :: Bool
  -- ^ whether to hide place name
  -> Bool
  -- ^ whether to hide transition name
  -> PreparedFont Double
  -- ^ the font to use
  -> (String, Maybe Int)
  -- ^ a node (the first part is used for its label)
  -> Point V2 Double
  -- ^ where to place the node
  -> Diagram B
drawNode _ hideTName pfont (l, Nothing) p  = place
  (addTName $ rect 20 20 # lwL 0.5 # named l # svgClass "rect")
  p
  where
    addTName
      | hideTName = id
      | otherwise = (center (text' pfont 18 l) `atop`)
drawNode hidePName _ pfont (l, Just i) p
  | i < 5
  = place (foldl' atop label $ [placeToken j | j <- [1..i]] ++ [emptyPlace]) p
  | otherwise
  = place
    (foldl' atop label [
        token # translate (r2 (spacer,0)),
        text' pfont 20 (show i) # translate (r2 (-spacer,-4)),
        emptyPlace
        ])
    p
  where
    spacer = 9
    emptyPlace = circle 20 # lwL 0.5 # named l # svgClass "node"
    label
      | hidePName = mempty
      | otherwise = center (text' pfont 18 l)
        # translate (r2 (0, - (3 * spacer)))
        # svgClass "nlabel"
    tokenGrey = sRGB24 136 136 136
    token = circle 4.5 # lc tokenGrey # fc tokenGrey # lwL 0 # svgClass "token"
    placeToken j = token
      # translate (r2 (8 * sqrt(fromIntegral (i - 1)), 0))
      # rotateBy (fromIntegral j / fromIntegral i)

{-|
Edges are drawn as arcs between nodes (identified by labels).
-}
drawEdge
  :: Bool
  -- ^ whether to hide weight of 1
  -> PreparedFont Double
  -- ^ the font to use
  -> Int
  -- ^ the edges label
  -> String
  -- ^ label of start node
  -> String
  -- ^ label of end node
  -> Path V2 Double
  -- ^ the path along which to align the edge
  -> Diagram B
  -- ^ the diagram which contains labelled nodes already
  -> Diagram B
drawEdge hide1 f l l1 l2 path d =
  let opts = with
        & arrowShaft .~ unLoc trail
        & arrowHead .~ arrowheadTriangle (150 @@ deg)
        & headGap .~ local 0.005
        & headLength .~ local 10
      labelPoint :: Point V2 Double
      labelPoint = trail `atParam` 0.4 .-^ 8 *^ n
        where
          n = trail `normalAtParam` 0.4
      addLabel
        | hide1 && l == 1 = id
        | otherwise = atop (place (centerXY $ text' f 20 $ show l) labelPoint # svgClass "elabel")
  in addLabel (connectOutside'' opts l1 l2 d # lwL 0.5) # svgClass "."
  where
    trail = trailBetween path l1 l2 d

renderWith
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => String
  -> String
  -> p n String
  -> DrawSettings
  -> m FilePath
renderWith path task net config =
  cacheNet (path ++ task) id net
    (not $ withPlaceNames config)
    (not $ withTransitionNames config)
    (not $ with1Weights config)
    (withGraphvizCommand config)
