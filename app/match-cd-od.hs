module Main (main) where

import Alloy.CdOd.Types                 (ClassConfig (..))
import Alloy.CdOd.MatchCdOd             (MatchCdOdConfig (..), matchCdOd)
import EvaluateArgs                     (evaluateArgs)

import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  let config = MatchCdOdConfig {
          classConfig = ClassConfig {
              classes      = (4, 4),
              aggregations = (0, Just 2),
              associations = (0, Just 2),
              compositions = (0, Just 1),
              inheritances = (1, Just 2)
            },
          maxObjects   = 4,
          maxInstances = Nothing,
          searchSpace  = 10
        }
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  matchCdOd config "" s seed >>= print
