module Modelling.PetriNet.Diagram (drawNet) where

import Modelling.PetriNet.Parser (
  parsePetriLike, petriLikeToGr, simpleNameMap
  )

import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.Graph.Inductive             (Gr)
import Diagrams.Backend.Rasterific             (B)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
import Data.GraphViz                    hiding (Path)
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call               (AlloyInstance)

-------------------------------------------------------------------------
drawNet ::
  String -> String -> AlloyInstance -> GraphvizCommand -> ExceptT String IO (Diagram B)
drawNet f t inst gc = do
  pl <- except $ parsePetriLike f t inst
  let bm = simpleNameMap pl
  gr    <- except $ petriLikeToGr pl bm
  graph <- lift $ GV.layoutGraph gc gr
  pfont <- lift $ lin
  return $ drawGraph pfont graph

drawGraph
  :: PreparedFont Double
  -> Gr (AttributeNode (String, Maybe Int)) (AttributeEdge String)
  -> Diagram B
drawGraph pfont graph = gedges # frame 1
  where
    (nodes, edges) = GV.getGraph graph
    gnodes = M.foldlWithKey (\g l p -> g `atop` drawNode pfont l p) mempty nodes
    gedges = foldl (\g (s, t, l, p) -> g # drawEdge pfont l s t p) gnodes edges

drawNode :: PreparedFont Double -> (String,Maybe Int) -> Point V2 Double -> Diagram B
drawNode pfont (l, Nothing) p  = place
  (center (text' pfont l)
    `atop` rect 20 20 # named l)
  p
drawNode pfont (l,Just i) p  = place
  (center (text' pfont l) # translate (r2(0,10))
    `atop` circle 3 #fc black # translate (r2(-5,-11))
    `atop` text' pfont (show i :: String) # translate (r2(1,-15))
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
