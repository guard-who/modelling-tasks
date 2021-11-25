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
import Data.List                        (foldl')
import Data.Maybe
import Diagrams.Backend.SVG             (B, svgClass, SVG)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call              (AlloyInstance, Object)
import Data.Data (Typeable)

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
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> GraphvizCommand
  -- ^ how to distribute the nodes
  -> ExceptT String IO (Diagram B)
drawNet labelOf pl hidePNames hideTNames hide1 gc = do
  gr    <- except $ left errorMessage $ petriLikeToGr pl
  graph <- lift $ GV.layoutGraph gc gr
  pfont <- lift lin
  return $ drawGraph labelOf hidePNames hideTNames hide1 pfont graph
  where
    errorMessage x =
      "drawNet: Could not find " ++ labelOf x ++ " within the graph"

getNet
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> ExceptT String IO (PetriLike String, t String)
getNet parseInst inst = do
  (net, rename) <-
    getNetWith "flow" "tokens" inst
  conc <- except $ parseInst inst
  rconc <- except $ traverse rename conc
  return (net, rconc)

getDefaultNet
  :: AlloyInstance
  -> ExceptT String IO (PetriLike String)
getDefaultNet inst= fst <$>
  getNetWith "defaultFlow" "defaultTokens" inst

{-|
Returns a Petri net like graph using 'parsePetriLike'.
It additionally parses another part of the instance.
All nodes are renamed using the 'simpleRenameWith' function.
The renaming is also applied to the additionally parsed instance.
-}
getNetWith
  :: String
  -- ^ flow
  -> String
  -- ^ tokens
  -> AlloyInstance
  -- ^ the instance to parse
  -> ExceptT String IO (PetriLike String, Object -> Either String String)
getNetWith f t inst = do
  pl <- except $ parsePetriLike f t inst
  let rename = simpleRenameWith pl
  pl' <- except $ traversePetriLike rename pl
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
      (\g (s, t, l, p) -> g # drawEdge hide1 pfont l (labelOnly s) (labelOnly t) p)
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
  (addTName $ rect 20 20 # named l # svgClass "rect")
  p
  where
    addTName
      | hideTName = id
      | otherwise = (center (text' pfont l) `atop`)
drawNode hidePName _ pfont (l, Just i) p
  | i < 5
  = place (foldl' atop label $ [placeToken j | j <- [1..i]] ++ [emptyPlace]) p
  | otherwise
  = place
    (foldl' atop label [
        token # translate (r2 (spacer,0)),
        text' pfont (show i) # translate (r2 (-spacer,-4)),
        emptyPlace
        ])
    p
  where
    spacer = 9
    emptyPlace = circle 20 # named l # svgClass "node"
    label
      | hidePName = mempty
      | otherwise = center (text' pfont l) # translate (r2 (0, -3 * spacer)) # svgClass "nlabel"
    token = circle 5 # fc black # svgClass "token"
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
  -- ^ the diagram which contains labeled nodes already
  -> Diagram B
drawEdge hide1 f l l1 l2 path d =
  let opts p = with & arrowShaft .~ (unLoc . head $ pathTrails p)
      points = concat $ pathPoints path
      labelPoint = points !! (length points `div` 2)
      addLabel
        | hide1 && l == 1 = id
        | otherwise = atop (place (text' f $ show l) labelPoint # svgClass "label")
  in addLabel (connectOutside'' (opts path) l1 l2 d) # svgClass "."

connectOutside'' ::  (IsName nm1, IsName nm2, RealFloat n,
                           Typeable n, Show n) =>
                          ArrowOpts n
                          -> nm1
                          -> nm2
                          -> QDiagram SVG V2 n Any
                          -> QDiagram SVG V2 n Any
connectOutside'' opts n1 n2 =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
        s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
        e' = fromMaybe (location b2) $ traceP midpoint v b2
    in
      atop (arrowBetween' opts s' e' # svgClass "edge")

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
