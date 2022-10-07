{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ByteString                  as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy             as LBS (fromStrict)
import qualified Data.ByteString.UTF8             as BS (fromString)
import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Modelling.Auxiliary.Common       (Object)
import Modelling.PetriNet.Parser (
  parsePetriLike,
  petriLikeToGr,
  simpleRenameWith,
  )
import Modelling.PetriNet.Reach.Group   (writeSVG)
import Modelling.PetriNet.Types (
  DrawSettings (..),
  Net (mapNet, traverseNet),
  PetriLike,
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
import Data.Data                        (Typeable)
import Data.Digest.Pure.SHA             (sha512, showDigest)
import Data.FileEmbed                   (embedFile)
import Data.List                        (foldl')
import Data.Maybe
import Diagrams.Backend.SVG             (B, svgClass, SVG)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
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
import Graphics.SVGFonts.ReadFont       (PreparedFont, loadFont')
import Language.Alloy.Call              (AlloyInstance)
import System.Directory                 (doesFileExist)

lin :: IO (PreparedFont Double)
lin = do
  let s = $(embedFile "fonts/LinLibertine.svg")
      (errors, font') = loadFont' "LinLibertine.svg" s
  when (errors /= "") (putStrLn errors)
  return font'

cacheNet
  :: (Net PetriLike n, Ord a)
  => String
  -> (a -> String)
  -> PetriLike n a
  -> Bool
  -> Bool
  -> Bool
  -> GraphvizCommand
  -> ExceptT String IO FilePath
cacheNet path labelOf pl hidePNames hideTNames hide1 gc = (svg <$) . cache $ do
  dia <- drawNet labelOf pl hidePNames hideTNames hide1 gc
  liftIO $ writeSVG svg dia
  where
    cache create = do
      let create' = create >> liftIO (BS.writeFile petri pl')
      isFile <- liftIO $ doesFileExist svg
      if isFile
        then do
          f <- liftIO $ BS.readFile petri
          when (f /= pl') $ do
            liftIO $ appendFile (path ++ "busted.txt") petriId
            create'
        else create'
    pl' = BS.fromString (show (mapNet labelOf pl))
    petriId = path ++ "petri" ++ showDigest (sha512 $ LBS.fromStrict pl')
    petri = petriId ++ ".hs"
    svg = petriId
      ++ short hidePNames
      ++ short hideTNames
      ++ short hide1
      ++ short gc
      ++ ".svg"

short :: Enum a => a -> String
short x = show $ fromEnum x

{-| Create a 'Diagram's graph of a Petri net like graph definition ('PetriLike')
by distributing places and transitions using GraphViz.
The provided 'GraphvizCommand' is used for this distribution.
-}
drawNet
  :: (Net PetriLike n, Ord a)
  => (a -> String)
  -- ^ how to obtain labels of the nodes
  -> PetriLike n a
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
  :: (Net PetriLike n, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> ExceptT String IO (PetriLike n String, t String)
getNet parseInst inst = do
  (net, rename) <-
    getNetWith "flow" "tokens" inst
  conc <- except $ parseInst inst
  rconc <- except $ traverse rename conc
  return (net, rconc)

getDefaultNet
  :: Net PetriLike n
  => AlloyInstance
  -> ExceptT String IO (PetriLike n String)
getDefaultNet inst= fst <$>
  getNetWith "defaultFlow" "defaultTokens" inst

{-|
Returns a Petri net like graph using 'parsePetriLike'.
It additionally parses another part of the instance.
All nodes are renamed using the 'simpleRenameWith' function.
The renaming is also applied to the additionally parsed instance.
-}
getNetWith
  :: Net PetriLike n
  => String
  -- ^ flow
  -> String
  -- ^ tokens
  -> AlloyInstance
  -- ^ the instance to parse
  -> ExceptT String IO (PetriLike n String, Object -> Either String String)
getNetWith f t inst = do
  pl <- except $ parsePetriLike f t inst
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
      (\g (s, t, l, p) -> g
        # drawEdge hide1 pfont l (labelOnly s) (labelOnly t) (nonEmpty s t p g)
      )
      gnodes
      edges
    withLabel = first labelOf
    labelOnly = labelOf . fst
    nonEmpty s t p g =
      let (x, y, z) = fromJust $ pointsFromTo (labelOnly s) (labelOnly t) g
      in case pathPoints p of
        [] -> fromVertices [x, y, z]
        _  -> p

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
    trail =
      let x = head $ pathTrails path
          points = head $ pathPoints path
          oldPos = head points
          oldE = last points
      in maybe
           x
           (\(pos, _, e) -> scaleAndPositionTrail pos e oldPos oldE x)
           $ pointsFromTo l1 l2 d

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

connectOutside'' ::  (IsName nm1, IsName nm2, RealFloat n,
                           Typeable n, Show n) =>
                          ArrowOpts n
                          -> nm1
                          -> nm2
                          -> QDiagram SVG V2 n Any
                          -> QDiagram SVG V2 n Any
connectOutside'' opts n1 n2 g =
  let (s', _, e') = fromJust $ pointsFromTo n1 n2 g
  in arrowBetween' opts s' e' # svgClass "edge"
     `atop` g

{-|
Render text as a diagram.
-}
text'
  :: PreparedFont Double
  -- ^ which font to use
  -> Double
  -- ^ font size
  -> String
  -- ^ what to write
  -> Diagram B
text' pfont s x = x
#if MIN_VERSION_SVGFonts(1,8,0)
  # svgText (def :: TextOpts Double) { textFont = pfont }
  # fit_height s
  # set_envelope
  # lw none
#else
  # textSVG_ (TextOpts pfont INSIDE_H KERN False s s)
#endif
  # fc black
  # lc black
  # lwL 0.4

renderWith
  :: (MonadIO m, Net PetriLike n, OutputMonad m)
  => String
  -> String
  -> PetriLike n String
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
