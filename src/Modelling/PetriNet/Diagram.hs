{-# LANGUAGE TemplateHaskell #-}

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

import qualified Control.Monad.Output             as OM (translate)
import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Modelling.Auxiliary.Common       (Object, cacheIO, short)
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
import Modelling.PetriNet.Reach.Group   (writeSVG)
import Modelling.PetriNet.Types (
  DrawSettings (..),
  Net (mapNet, traverseNet),
  )

import Control.Arrow                    (ArrowChoice(left), first)
import Control.Monad                    (when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  LangM',
  OutputMonad (..),
  english,
  german,
  )
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except, runExceptT)
import Data.Graph.Inductive             (Gr)
import Data.GraphViz                    hiding (Path)
import Data.FileEmbed                   (embedFile)
import Data.List                        (foldl')
import Diagrams.Backend.SVG             (B, svgClass)
import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont       (PreparedFont, loadFont')
import Language.Alloy.Call              (AlloyInstance)

lin :: IO (PreparedFont Double)
lin = do
  let s = $(embedFile "fonts/LinLibertine.svg")
      (errors, font') = loadFont' "LinLibertine.svg" s
  when (errors /= "") (putStrLn errors)
  return font'

cacheNet
  :: (Net p n)
  => String
  -> (a -> String)
  -> p n a
  -> Bool
  -> Bool
  -> Bool
  -> GraphvizCommand
  -> ExceptT String IO FilePath
cacheNet path labelOf pl hidePNames hideTNames hide1 gc =
  cacheIO path ext "petri" (mapNet labelOf pl) $ \svg pl' -> do
    dia <- drawNet id pl' hidePNames hideTNames hide1 gc
    liftIO $ writeSVG svg dia
  where
    ext = short hidePNames
      ++ short hideTNames
      ++ short hide1
      ++ short gc
      ++ ".svg"

{-| Create a 'Diagram's graph of a Petri net like graph definition ('Net')
by distributing places and transitions using GraphViz.
The provided 'GraphvizCommand' is used for this distribution.
-}
drawNet
  :: (Net p n, Ord a)
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
  -> ExceptT String IO (Diagram B)
drawNet labelOf pl hidePNames hideTNames hide1 gc = do
  gr    <- except $ left errorMessage $ netToGr pl
  graph <- lift $ GV.layoutGraph gc gr
  pfont <- lift lin
  return $ drawGraph labelOf hidePNames hideTNames hide1 pfont graph
  where
    errorMessage x =
      "drawNet: Could not find " ++ labelOf x ++ " within the graph"

getNet
  :: (Net p n, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> ExceptT String IO (p n String, t String)
getNet parseInst inst = do
  (net, rename) <-
    getNetWith "flow" "tokens" inst
  conc <- except $ parseInst inst
  rconc <- except $ traverse rename conc
  return (net, rconc)

getDefaultNet
  :: Net p n
  => AlloyInstance
  -> ExceptT String IO (p n String)
getDefaultNet inst= fst <$>
  getNetWith "defaultFlow" "defaultTokens" inst

{-|
Returns a Petri net like graph using 'parseNet'.
It additionally parses another part of the instance.
All nodes are renamed using the 'simpleRenameWith' function.
The renaming is also applied to the additionally parsed instance.
-}
getNetWith
  :: Net p n
  => String
  -- ^ flow
  -> String
  -- ^ tokens
  -> AlloyInstance
  -- ^ the instance to parse
  -> ExceptT String IO (p n String, Object -> Either String String)
getNetWith f t inst = do
  pl <- except $ parseNet f t inst
  let rename = simpleRenameWith pl
  pl' <- except $ traverseNet rename pl
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
      | otherwise = center (text' pfont 18 l) # translate (r2 (0, -3 * spacer)) # svgClass "nlabel"
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
  :: (MonadIO m, Net p n, OutputMonad m)
  => String
  -> String
  -> p n String
  -> DrawSettings
  -> LangM' m FilePath
renderWith path task net config = do
  f <- lift $ liftIO $ runExceptT $
    cacheNet (path ++ task) id net
      (not $ withPlaceNames config)
      (not $ withTransitionNames config)
      (not $ with1Weights config)
      (withGraphvizCommand config)
  either
    (const $ (>> return "") $ refuse $ OM.translate $ do
      english "Drawing diagram failed!"
      german "Diagrammzeichnen fehlgeschlagen!"
    )
    return
    f
