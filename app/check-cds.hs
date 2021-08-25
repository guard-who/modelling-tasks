{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import qualified Data.Map                         as M (empty, insert)
import qualified Language.Alloy.Call              as Alloy (getInstances)

import Modelling.CdOd.CD2Alloy.Transform (createRunCommand, mergeParts, transform)
import Modelling.CdOd.Edges             (fromEdges)
import Modelling.CdOd.Output            (drawCdFromSyntax, drawOdFromInstance)
import Modelling.CdOd.Types
  (AssociationType (..), Connection (..), DiagramEdge, Syntax, toOldSyntax)

import Data.GraphViz                    (DirType (..), GraphvizOutput (Pdf))
import Data.Map                         (Map)

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

a :: DiagramEdge
a = ("A", "B", Assoc Association "a" (1, Just 1) (1, Just 1) False)

b :: DiagramEdge
b = ("C", "B", Assoc Association "b" (1, Just 1) (1, Just 1) False)


main :: IO ()
main = do
  -- names are required
  let cd0 = fromEdges ["A", "B", "C"] [a, b, inh1]
      cd1 = fromEdges ["A", "B", "C"] [a, inh1]
      cd2 = fromEdges ["A", "B", "C"] [b, inh1]
      dirs = M.insert "a" Forward $ M.insert "b" Forward M.empty
  drawCdAndOdsFor Nothing "names" dirs [cd0, cd1, cd2] "cd1 and cd2"
  drawCdAndOdsFor Nothing "names" dirs [cd0, cd1, cd2] "cd1 and not cd2"
  drawCdAndOdsFor Nothing "names" dirs [cd0, cd1, cd2] "cd2 and not cd1"
  drawCdAndOdsFor Nothing "names" dirs [cd0, cd1, cd2] "cd0 and not cd1 and not cd2"
  -- no matter where limits
  drawCdsAndOds "y-limits-" y' z'
  drawCdsAndOds "z-limits-" y'' z''

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
  in foldr ((>>) . (\(i, cd) -> drawCdAndOdsFor (Just 1) (c ++ show i) M.empty [cd] "cd0")) (return ()) $ zip [0..] cds
  where
    classes = map (:[]) ['A' .. 'D']

drawCdAndOdsFor
  :: Maybe Integer
  -> String
  -> Map String DirType
  -> [Syntax]
  -> String
  -> IO ()
drawCdAndOdsFor is c dirs cds cmd = do
  mapM_ (\(cd, i) -> drawCdFromSyntax True True Nothing cd (c ++ "-cd" ++ show i) Pdf >>= print) $ zip cds [0..]
  let parts' = combineParts (foldr mergeParts (head parts) $ tail parts)
        ++ createRunCommand Nothing cmd 3 3
  ods <- Alloy.getInstances is parts'
  mapM_ (\(od, i) -> drawOdFromInstance od Nothing dirs True (c ++ '-' : shorten cmd ++ "-od" ++ show i) Pdf >>= print)
    $ zip (maybe id (take . fromInteger) is ods) [1..]
  where
    parts = zipWith (\cd i -> getFour $ transform (toOldSyntax cd) Nothing (show i) "") cds [0..]
    getFour (p1, p2, p3, p4, _) = (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) =
      p1 ++ p2 ++ p3 ++ p4
    shorten (' ':'a':'n':'d':' ':'c':'d':ys) =
      "and" ++ shorten ys
    shorten (' ':'a':'n':'d':' ':'n':'o':'t':' ':'c':'d':ys) =
      "not" ++ shorten ys
    shorten (y:ys) = y : shorten ys
    shorten []     = []
