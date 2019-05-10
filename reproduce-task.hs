{-# LANGUAGE TupleSections #-}
module Main (main) where

import Output     (drawCdFromSyntax, drawOdFromInstance)
import Types      (ClassConfig (..))
import RandomTask (getRandomTask)

import Control.Monad.Random (evalRandT, mkStdGen)
import Data.ByteString      (ByteString)
import Data.Digest.CRC32    (crc32)
import Data.Function        (on)
import Data.GraphViz        (GraphvizOutput (Pdf))
import Data.List            ((\\), groupBy, intercalate, sortBy)
import Data.String          (fromString)
import System.Environment   (getArgs)

import qualified Data.Map as M (traverseWithKey)

main :: IO ()
main = do
  [task] <- getArgs
  reproduceTask task

reproduceTask :: String -> IO ()
reproduceTask task = do
  let config = ClassConfig {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Just 2),
          associations = (Nothing, Just 2),
          compositions = (Nothing, Just 1),
          inheritances = (Just 1, Just 2)
        }
  let maxObjects = 4
  let g = mkStdGen $ fromIntegral $ crc32 (fromString task :: ByteString)
  (cds, instas) <- evalRandT (getRandomTask config maxObjects 10 (-1)) g
  (\i cd -> drawCdFromSyntax True False Nothing cd (output ++ '-' : show i) Pdf) `M.traverseWithKey` cds
  uncurry drawOd `mapM_` concat (zip [1 :: Int ..] <$> groupBy ((==) `on` fst) (sortBy (compare `on` fst) instas))
  where
    output = "output"
    drawOd x (y, insta) =
      drawOdFromInstance True insta (output ++ '-' : toDescription y 2 ++ '-' : show x) Pdf
    toDescription :: [Int] -> Int -> String
    toDescription x n =
      intercalate "and" (show <$> x) ++ foldr ((++) . ("not" ++) . show) [] ([1..n] \\ x)
