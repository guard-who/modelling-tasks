module Main (main) where

import Types
import Generate
import Transform (transform)

main :: IO ()
main =
  generate Config {
      classes      = (Just 4, Just 4),
      aggregations = (Nothing, Nothing),
      associations = (Nothing, Nothing),
      compositions = (Nothing, Nothing),
      inheritances = (Nothing, Nothing),
      searchSpace  = 10
    }
  >>= \syntax -> transform syntax (Just "output") True ""

