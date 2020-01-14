module Main (main) where

import Output    (drawCdFromSyntax, drawOdFromInstance)
import Types     (ClassConfig (..))
import MatchCdOd (getRandomTask)

import Control.Monad.Random (StdGen, evalRandT, getStdGen, mkStdGen)
import Data.ByteString      (ByteString)
import Data.Digest.CRC32    (crc32)
import Data.Function        (on)
import Data.GraphViz        (GraphvizOutput (Pdf))
import Data.List            ((\\), groupBy, intercalate, sortBy)
import Data.Map             (empty)
import Data.String          (fromString)
import System.Environment   (getArgs)

import qualified Data.Map as M (traverseWithKey)

main :: IO ()
main = do
  args <- getArgs
  g <- case args of
    []     -> getStdGen
    [task] -> return $ mkStdGen $ fromIntegral $ crc32 (fromString task :: ByteString)
    _      -> error "Too many arguments"
  reproduceTask g

reproduceTask :: StdGen -> IO ()
reproduceTask g = do
  let config = ClassConfig {
          classes      = (4, 4),
          aggregations = (0, Just 2),
          associations = (0, Just 2),
          compositions = (0, Just 1),
          inheritances = (1, Just 2)
        }
  let maxObjects = 4
  (cds, instas) <- evalRandT (getRandomTask config maxObjects 10 Nothing) g
  (\i cd -> drawCdFromSyntax False True Nothing cd (output ++ '-' : show i) Pdf) `M.traverseWithKey` cds
  uncurry drawOd `mapM_` concat (zip [1 :: Int ..] <$> groupBy ((==) `on` fst) (sortBy (compare `on` fst) instas))
  where
    output = "output"
    drawOd x (y, insta) =
      drawOdFromInstance insta empty True (output ++ '-' : toDescription y 2 ++ '-' : show x) Pdf
    toDescription :: [Int] -> Int -> String
    toDescription x n =
      intercalate "and" (show <$> x) ++ concatMap (("not" ++) . show) ([1..n] \\ x)
