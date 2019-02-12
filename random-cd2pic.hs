module Main (main) where

import Types
import Generate
import Output

import Data.GraphViz

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
  >>= drawCdFromSyntax "output" Pdf

