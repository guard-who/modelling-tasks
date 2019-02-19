module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (getAllMutationResults)
import Output    (drawCdFromSyntax, drawOdFromInstance)
import Transform (transform)
import Types     (Config (..))

import Control.Monad       (unless)
import Data.List           (intercalate, union)
import Data.List.Split     (splitOn)
import Data.GraphViz       (GraphvizOutput (Pdf))
import Data.Time.LocalTime (getZonedTime)

import System.FilePath       (searchPathSeparator)
import System.IO
import System.Process
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  let config = Config {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Nothing),
          associations = (Nothing, Nothing),
          compositions = (Nothing, Nothing),
          inheritances = (Nothing, Nothing),
          searchSpace  = 10,
          output       = "output",
          maxInstances = -1
        }
  (names, edges) <- generate config
  let cd1 = fromEdges names edges
  drawCdFromSyntax cd1 (output config ++ "1") Pdf
  unless (anyRedEdge cd1) $ do
    time <- getZonedTime
    let (part1, part2, part3, part4, part5) = transform cd1 "1" (show time)
        als1 = part1 ++ part2 ++ part3 ++ part4 ++ part5
    instances1 <- getAlloyInstances (maxInstances config) als1
    unless (null instances1) $ do
      mutations <- shuffleM $ getAllMutationResults names edges
      let cd2 = fromEdges names $ getFirstValid names mutations
          (part1', part2', part3', part4', part5') = transform cd2 "2" (show time)
          als2  = part1' ++ part2' ++ part3' ++ part4' ++ part5'
          als12 = part1 ++ (part2 `unionL` part2') ++ (part3 `unionL` part3')
                  ++ part4 ++ part4' ++ "run { cd1 and (not cd2) } for 5"
      drawCdFromSyntax cd2 (output config ++ "2") Pdf
      instances2  <- getAlloyInstances (maxInstances config) als2
      unless (null instances2) $ do
        instances12 <- getAlloyInstances (maxInstances config) als12
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

getAlloyInstances :: Int -> String -> IO [String]
getAlloyInstances maxInsta content = do
  let callAlloy = proc "java" ["-cp", '.' : searchPathSeparator :  "alloy/Alloy-5.0.0.1.jar",
                               "alloy.RunAlloy", show maxInsta]
  (Just hin, Just hout, _, _) <- createProcess callAlloy { std_out = CreatePipe, std_in = CreatePipe }
  hPutStr hin content
  hClose hin
  fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  where
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> hGetLine h <*> getWholeOutput h
