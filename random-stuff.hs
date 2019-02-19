module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (getAllMutationResults)
import Output    (drawCdFromSyntax, drawOdFromInstance)
import Transform (transform)
import Types     (ClassConfig (..))

import qualified Alloy

import Control.Monad       (unless)
import Data.List           (union)
import Data.GraphViz       (GraphvizOutput (Pdf))

import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Nothing),
          associations = (Nothing, Nothing),
          compositions = (Nothing, Nothing),
          inheritances = (Nothing, Nothing),
          searchSpace  = 10
        }
  (names, edges) <- generate config
  let output = "output"
  let maxInstances = -1
  let cd1 = fromEdges names edges
  drawCdFromSyntax cd1 (output ++ "1") Pdf
  unless (anyRedEdge cd1) $ do
    let (part1, part2, part3, part4, part5) = transform cd1 "1" ""
        als1 = part1 ++ part2 ++ part3 ++ part4 ++ part5
    instances1 <- Alloy.getInstances maxInstances als1
    unless (null instances1) $ do
      mutations <- shuffleM $ getAllMutationResults names edges
      let cd2 = fromEdges names $ getFirstValid names mutations
          (part1', part2', part3', part4', part5') = transform cd2 "2" ""
          als2  = part1' ++ part2' ++ part3' ++ part4' ++ part5'
          als12 = part1 ++ (part2 `unionL` part2') ++ (part3 `unionL` part3')
                  ++ part4 ++ part4' ++ "run { cd1 and (not cd2) } for 5"
      drawCdFromSyntax cd2 (output ++ "2") Pdf
      instances2  <- Alloy.getInstances maxInstances als2
      unless (null instances2) $ do
        instances12 <- Alloy.getInstances maxInstances als12
        mapM_ (\(i, insta) -> drawOdFromInstance insta (show i) Pdf) (zip [1 :: Integer ..] instances12)
  where
    unionL x y = unlines $ lines x `union` lines y

getFirstValid :: [String] -> [[DiagramEdge]] -> [DiagramEdge]
getFirstValid _     []
  = error "There is no (further) valid mutation for this chart!"
getFirstValid names (x:xs)
  | checkMultiEdge x, not (anyRedEdge $ fromEdges names x)
  = x
  | otherwise
  = getFirstValid names xs
