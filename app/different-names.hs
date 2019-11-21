module Main (main) where

import qualified Data.Bimap as BM (lookupR)

import Edges      (toEdges)
import NaiveTasks (getDifferentNamesTask)
import Output     (drawCdFromSyntax, drawOdFromInstance)
import Types      (AssociationType (..), ClassConfig (..), Connection (..))

import Control.Monad.Random (evalRandT, getStdGen)
import Data.GraphViz        (DirType (..), GraphvizOutput (Pdf))
import Data.Map             (empty, insert)
import System.Environment   (getArgs)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Just 2),
          associations = (Nothing, Just 2),
          compositions = (Nothing, Just 2),
          inheritances = (Just 1, Just 2)
        }
  let maxObjects = 4
  args <- getArgs
  g    <- case args of
    []   -> getStdGen
    [g'] -> return $ read g'
    _    -> error "Too many arguments"
  putStrLn $ "Seed: " ++ show (show g)
  (cd, od, bm) <- evalRandT (getDifferentNamesTask config maxObjects 10 (-1)) g
  let backwards   = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t /= Association
                       , n <- BM.lookupR n' bm]
      forwards    = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t == Association
                       , n <- BM.lookupR n' bm]
      navigations = foldr (`insert` Back)
                          (foldr (`insert` Forward) empty forwards)
                          backwards
  drawCdFromSyntax True True Nothing cd (output ++ "-cd") Pdf
  drawOdFromInstance navigations True od (output ++ "-od") Pdf
  print bm
  where
    output = "output"
