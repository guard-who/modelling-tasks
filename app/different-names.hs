module Main (main) where

import qualified Data.Bimap as BM (lookupR)

import Alloy.CdOd.Edges      (toEdges)
import Alloy.CdOd.NaiveTasks (getDifferentNamesTask)
import Alloy.CdOd.Output     (drawCdFromSyntax, drawOdFromInstance)
import Alloy.CdOd.Types      (AssociationType (..), ClassConfig (..), Connection (..))

import Control.Monad.Random (evalRandT, getStdGen)
import Data.GraphViz        (DirType (..), GraphvizOutput (Pdf))
import Data.Map             (empty, insert)
import System.Environment   (getArgs)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (4, 4),
          aggregations = (0, Just 2),
          associations = (0, Just 2),
          compositions = (0, Just 2),
          inheritances = (1, Just 2)
        }
  let maxObjects = 4
  args <- getArgs
  g    <- case args of
    []   -> getStdGen
    [g'] -> return $ read g'
    _    -> error "Too many arguments"
  putStrLn $ "Seed: " ++ show (show g)
  (cd, od, bm) <- evalRandT (getDifferentNamesTask config maxObjects 10 Nothing) g
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
  drawOdFromInstance od navigations True (output ++ "-od") Pdf
  print bm
  where
    output = "output"
