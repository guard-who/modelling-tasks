module Main (main) where

import Types
import Generate
import Transform (transform)
import Output

import Data.GraphViz

main :: IO ()
main = do
 syntax <-
  generate Config {
      classes      = (Just 4, Just 4),
      aggregations = (Nothing, Nothing),
      associations = (Nothing, Nothing),
      compositions = (Nothing, Nothing),
      inheritances = (Nothing, Nothing),
      searchSpace  = 10
    }
 let output = "output"
 drawCdFromSyntax output Pdf syntax
 (part1, part2, part3, part4, part5) <- transform syntax ""
 let out = output ++ ".als" in writeFile out (part1 ++ part2 ++ part3 ++ part4 ++ part5) >> putStrLn ("More output written to " ++ out)
