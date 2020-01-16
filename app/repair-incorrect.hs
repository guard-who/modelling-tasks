module Main where

import Alloy.CdOd.NaiveTasks            (phraseChange, repairIncorrect)
import Alloy.CdOd.Output                (drawCdFromSyntax)
import Alloy.CdOd.Types                 (ClassConfig (..))

import Control.Monad.Random             (evalRandT, getStdGen)
import Data.GraphViz                    (GraphvizOutput (Pdf))
import System.Environment               (getArgs)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (4, 4),
          aggregations = (0, Just 2),
          associations = (0, Just 2),
          compositions = (0, Just 3),
          inheritances = (1, Just 3)
        }
  args <- getArgs
  g    <- case args of
    []   -> getStdGen
    [g'] -> return $ read g'
    _    -> error "Too many arguments"
  putStrLn $ "Seed: " ++ show (show g)
  (cd, chs) <- evalRandT (repairIncorrect config) g
  drawCdFromSyntax True True Nothing cd "cd" Pdf
  print $ fst <$> chs
  print $ phraseChange . snd <$> chs
