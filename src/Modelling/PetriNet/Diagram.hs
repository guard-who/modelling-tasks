{-|
Provides the ability to render Petri nets.
-}
module Modelling.PetriNet.Diagram (
  drawNet,
  getDefaultNet,
  getNet,
  ) where

import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Modelling.PetriNet.Parser (
  parsePetriLike,
  petriLikeToGr,
  simpleRenameWith,
  )
import Modelling.PetriNet.Types         (PetriLike, traversePetriLike)

import Control.Arrow                    (ArrowChoice(left), first)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.Graph.Inductive             (Gr)
import Data.GraphViz                    hiding (Path)
import Diagrams.Backend.SVG             (B)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call              (AlloyInstance, Object)

{-| Create a 'Diagram's graph of a Petri net like graph definition ('PetriLike')
by distributing places and transitions using GraphViz.
The provided 'GraphvizCommand' is used for this distribution.
-}
drawNet
  :: Ord a
  => (a -> String)
  -- ^ how to obtain labels of the nodes
  -> PetriLike a
  -- ^ the graph definition
  -> GraphvizCommand
  -- ^ how to distribute the nodes
  -> ExceptT String IO (Diagram B)
drawNet labelOf pl gc = do
  gr    <- except $ left errorMessage $ petriLikeToGr pl
  graph <- lift $ GV.layoutGraph gc gr
  pfont <- lift lin
  return $ drawGraph labelOf pfont graph
  where
    errorMessage x =
      "drawNet: Could not find " ++ labelOf x ++ " within the graph"

getNet
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, t String)
getNet parseInst inst gc = do
  (net, rename) <- getNetWith "flow" "tokens" inst gc
  conc <- except $ parseInst inst
  rconc <- except $ traverse rename conc
  return (net, rconc)

getDefaultNet
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (QDiagram B V2 Double Any)
getDefaultNet inst gc = fst <$>
  getNetWith "defaultFlow" "defaultTokens" inst gc

{-|
Draws a Petri net like graph using 'drawNet'.
It additionally parses another part of the instance.
All nodes are renamed using the 'simpleRenameWith' function.
The renaming is also applied to the additionally parsed instance, that is why
this instance needs to be 'Traversable'.
-}
getNetWith
  :: String
  -- ^ flow
  -> String
  -- ^ tokens
  -> AlloyInstance
  -- ^ the instance to parse
  -> GraphvizCommand
  -- ^ how to draw the graph
  -> ExceptT String IO (Diagram B, Object -> Either String String)
getNetWith f t inst gc = do
  pl <- except $ parsePetriLike f t inst
  let rename = simpleRenameWith pl
  pl' <- except $ traversePetriLike rename pl
  dia <- drawNet id pl' gc
  return (dia, rename)

{-|
Obtain the Petri net like graph by drawing Nodes and connections between them
using the specific functions @drawNode@ and @drawEdge@.
-}
drawGraph
  :: (Ord a, Show b)
  => (a -> String)
  -- ^ how to obtain labels from nodes
  -> PreparedFont Double
  -- ^ the font to be used for labels
  -> Gr (AttributeNode (a, Maybe Int)) (AttributeEdge b)
  -- ^ the graph consisting of nodes and edges
  -> Diagram B
drawGraph labelOf pfont graph = gedges # frame 1
  where
    (nodes, edges) = GV.getGraph graph
    gnodes = M.foldlWithKey
      (\g l p -> g `atop` drawNode pfont (withLabel l) p)
      mempty
      nodes
    gedges = foldl
      (\g (s, t, l, p) -> g # drawEdge pfont l (labelOnly s) (labelOnly t) p)
      gnodes
      edges
    withLabel = first labelOf
    labelOnly = labelOf . fst

{-|
Nodes are either Places (having 'Just' tokens), or Transitions (having
'Nothing').
Transitions are drawn as squares.
Places are drawn as circles.
Each node gets a label.
-}
drawNode
  :: PreparedFont Double
  -- ^ the font to use
  -> (String, Maybe Int)
  -- ^ a node (the first part is used for its label)
  -> Point V2 Double
  -- ^ where to place the node
  -> Diagram B
drawNode pfont (l, Nothing) p  = place
  (center (text' pfont l)
    `atop` rect 20 20 # named l)
  p
drawNode pfont (l,Just i) p  = place
  (center (text' pfont l) # translate (r2(0,10))
    `atop` circle 3 # fc black # translate (r2 (5,-11))
    `atop` text' pfont (show i) # translate (r2 (-8,-14.7))
    `atop` circle 20 # named l)
  p

{-|
Edges are drawn as arcs between nodes (identified by labels).
-}
drawEdge
  :: Show a
  => PreparedFont Double
  -- ^ the font to use
  -> a
  -- ^ the edges label
  -> String
  -- ^ label of start node
  -> String
  -- ^ label of end node
  -> Path V2 Double
  -- ^ the path along which to align the edge
  -> Diagram B
  -- ^ the diagram which contains labeled nodes already
  -> Diagram B
drawEdge f l l1 l2 path d =
  let opts p = with & arrowShaft .~ (unLoc . head $ pathTrails p)
      points = concat $ pathPoints path
      labelPoint = points !! (length points `div` 2)
  in connectOutside' (opts path) l1 l2 d
     `atop` place (text' f $ show l) labelPoint

{-|
Render text as a diagram.
-}
text'
  :: PreparedFont Double
  -- ^ which font to use
  -> String
  -- ^ what to write
  -> Diagram B
text' pfont =
  textSVG_ (TextOpts pfont INSIDE_H KERN False 18 18)
  # fc black
  # lc black
