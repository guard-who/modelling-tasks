{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.Diagram (drawNet) where

--import Modelling.PetriNet.Types
import Modelling.PetriNet.Parser        (convertGr)

import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Diagrams.Backend.Rasterific             (B)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
-- import Data.Graph.Inductive.Graph       (mkGraph)
-- import Data.Graph.Inductive.PatriciaTree
  -- (Gr)
import Data.GraphViz                    hiding (Path)
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call               (AlloyInstance)

-------------------------------------------------------------------------
drawNet :: 
  String -> [(Int,(String, Maybe Int))] -> AlloyInstance -> GraphvizCommand -> IO (Diagram B)
drawNet f n inst gc =
  case convertGr f n inst of
    Left merror -> error merror
    Right gnet -> do
      graph <- GV.layoutGraph gc gnet
      pfont <- lin
      let (nodes, edges) = GV.getGraph graph
          gnodes = M.foldlWithKey (\g l p -> g `atop` drawNode pfont l p) mempty nodes
          gedges = foldl (\g (l1, l2, l, p) -> g # drawEdge pfont l l1 l2 p) gnodes edges
      return (gedges # frame 1)
      

drawNode :: PreparedFont Double -> (String,Maybe Int) -> Point V2 Double -> Diagram B
drawNode pfont (l, Nothing) p  = place
  (center (text' pfont l)
    `atop` rect 20 20 # named l)
  p
drawNode pfont (l,Just i) p  = place
  (center (text' pfont l)
    `atop` text' pfont (show i :: String) # translate (r2(-3,-15))
    `atop` circle 20 # named l)
  p

drawEdge :: PreparedFont Double -> String -> (String,Maybe Int) -> (String,Maybe Int) -> Path V2 Double -> Diagram B -> Diagram B
drawEdge f l (l1,_) (l2,_) path d = 
  let opts p = with & arrowShaft .~ (unLoc . head $ pathTrails p)
      points = concat $ pathPoints path
      labelPoint = points !! (length points `div` 2)
  in connectOutside' (opts path) l1 l2 d
     `atop` place (text' f l) labelPoint

text' :: PreparedFont Double -> String -> Diagram B
text' pfont t =
  textSVG_ (TextOpts pfont INSIDE_H KERN False 18 18) t
  # fc black
  # lc black

-----------------------------------------------------------------------
-- renderNet :: String -> Petri -> GraphvizCommand -> IO ()
-- renderNet name petri gc = do
  -- diagram <- drawNet petri gc
  -- renderSVG (name++".svg") (mkWidth 200) diagram
  -- print "PetriNetz erstellt"
