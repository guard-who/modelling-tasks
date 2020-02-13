module Main where

import qualified Data.Map                         as M (empty)
import qualified Language.Alloy.Call              as Alloy (getInstances)

import Alloy.CdOd.CD2Alloy.Transform    (transform)
import Alloy.CdOd.Edges                 (fromEdges)
import Alloy.CdOd.Output                (drawCdFromSyntax, drawOdFromInstance)
import Alloy.CdOd.Types
  (AssociationType (..), Connection (..), DiagramEdge, Syntax)

import Control.Arrow                    (first, second)
import Data.GraphViz                    (GraphvizOutput (Pdf))
import Data.Maybe                       (listToMaybe)

v :: DiagramEdge
v = ("C", "B", Assoc Aggregation "v" (0, Nothing) (1, Just 1) False)

w :: DiagramEdge
w = ("A", "C", Assoc Association "w" (-1, Just 2) (-1, Just 2) False)

x :: DiagramEdge
x = ("C", "B", Assoc Composition "x" (0, Just 1) (1, Just 1) False)

y' :: DiagramEdge
y' = ("D", "A", Assoc Association "y" (0, Just 1) (0, Nothing) False)

y'' :: DiagramEdge
y'' = ("D", "A", Assoc Association "y" (0, Nothing) (0, Nothing) False)

z' :: DiagramEdge
z' = ("D", "C", Assoc Aggregation "z" (0, Nothing) (2, Nothing) False)

z'' :: DiagramEdge
z'' = ("D", "C", Assoc Aggregation "z" (0, Just 1) (2, Nothing) False)

inh1 :: DiagramEdge
inh1 = ("A", "C", Inheritance)

inh2 :: DiagramEdge
inh2 = ("B", "A", Inheritance)

inh3 :: DiagramEdge
inh3 = ("C", "A", Inheritance)

main :: IO ()
main = do
  drawCdsAndOds "y" y' z'
  drawCdsAndOds "z" y'' z''

drawCdsAndOds
  :: String
  -> DiagramEdge
  -> DiagramEdge
  -> IO ()
drawCdsAndOds c y z =
  let cds = [fromEdges classes [x, y, z, inh1, inh2],
             fromEdges classes [v, x, y, z, inh1, inh2],
             fromEdges classes [x, y, z, inh2],
             fromEdges classes [x, y, z, inh3, inh2],
             fromEdges classes [w, x, y, z, inh2]]
  in foldr (id . (>>) . uncurry (drawCdAndOdFor c)) (return ()) $ zip cds [0..]
  where
    classes = ((:[]) <$> ['A' .. 'D'])

drawCdAndOdFor :: String -> Syntax -> Int -> IO ()
drawCdAndOdFor c cd n' = do
  drawCdFromSyntax True True Nothing cd (c ++ "-limits-cd" ++ n) Pdf
  (od0:_) <- Alloy.getInstances (Just 1) (combineParts $ transform (toOldSyntax cd) n "")
  drawOdFromInstance od0 M.empty True (c ++ "-limits-od" ++ n) Pdf
  where
    n = show n'
    combineParts (p1, p2, p3, p4, p5) =
      p1 ++ p2 ++ p3 ++ p4 ++ p5
    toOldSyntax = first (second listToMaybe <$>)
