{-# LANGUAGE NamedFieldPuns #-}

module PetriDiagram where

import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.GraphViz                    hiding (Path)
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import AuxFunctions
  
defaultPetri :: Petri
defaultPetri = Petri
  { startM = [1,1,0]
  , trans = [([1,0,0],[0,1,0]),([1,0,0],[0,0,1]),([0,1,1],[2,0,0])]
  }
  
testPrep :: IO ()
testPrep = do
  print $ prepNet defaultPetri  
----------------------Preparing a PetriNet for Graph--------------------
prepNet :: Petri -> Gr String String
prepNet Petri{startM,trans} = mkGraph (prepNodes "s" 1 (length startM) nA 0) (prepEdges (length startM) trans)
  where nA = (length startM)+(length trans)
        
prepNodes :: String -> Int -> Int -> Int -> Int -> [(Int,String)]
prepNodes s n h max i 
-- | max > i > h   = ((i,"t"++n):prepNHelper (n+1) h max (i+1))
 | i == h  = ((i,"t"++(show(n-i))):prepNodes "t" (n-i+1) h max (i+1))
 | max > i       = ((i,s++(show n)):prepNodes s (n+1) h max (i+1))
 | otherwise     = []
 
--Counter-> transitions -> Ausgabe
prepEdges :: Int -> [Trans] -> [(Int,Int,String)]
prepEdges _ [] = []
prepEdges ex ((pre,post):rt) = createPre ex 0 pre
                            ++ createPost ex 0 post
                            ++ prepEdges (ex+1) rt

--ExternCounter -> InternCounter->List->Ausgabe
createPre :: Int -> Int -> Mark -> [(Int,Int,String)]
createPre _ _ [] = []
createPre ex i (m:rm) 
 | m /= 0    = ((i,ex,show m):createPre ex (i+1) rm)
 | otherwise =  createPre ex (i+1) rm

createPost :: Int -> Int -> Mark -> [(Int,Int,String)]
createPost _ _ [] = []
createPost ex i (m:rm) 
 | m /= 0    = ((ex,i,show m):createPost ex (i+1) rm)
 | otherwise = createPost ex (i+1) rm

-------------------------------------------------------------------------
drawNet :: Gr String String -> IO (Diagram B)
drawNet pnet = do
  graph <- GV.layoutGraph Dot pnet
  pfont <- lin
  let (nodes, edges) = GV.getGraph graph
      gnodes = M.foldlWithKey (\g l p -> g `atop` drawNode pfont l p) mempty nodes
      gedges = foldl (\g (l1, l2, l, p) -> g # drawEdge pfont l l1 l2 p) gnodes edges
  return (gedges # frame 1)

net :: Gr String String
net = mkGraph
  [(0,"s1"),
   (1, "t1"),
   (2,"s2"),
   (3, "t2")]
  ([(0,1,"1"),
   (1,2,"1"),
   (0,3,"1"),
   (2,3,"1")]
  )

drawNode :: PreparedFont Double -> String -> Point V2 Double -> Diagram B
drawNode pfont l p = place
  (center (text' pfont l)
    `atop` circle 20 # named l)
  p

drawEdge :: PreparedFont Double -> String -> String -> String -> Path V2 Double -> Diagram B -> Diagram B
drawEdge f l l1 l2 path d = 
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

renderNet :: Petri -> IO ()
renderNet petri = do
  diagram <- drawNet (prepNet petri)
  renderSVG "example.svg" (mkWidth 200) diagram
  print $ "PetriNetz erstellt"
