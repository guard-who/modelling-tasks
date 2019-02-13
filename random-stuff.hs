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
 let out = output ++ ".part1" in writeFile out part1 >> putStrLn ("Some output written to " ++ out)
 let out = output ++ ".part2" in writeFile out part2 >> putStrLn ("Some output written to " ++ out)
 let out = output ++ ".part3" in writeFile out part3 >> putStrLn ("Some output written to " ++ out)
 let out = output ++ ".part4" in writeFile out part4 >> putStrLn ("Some output written to " ++ out)
 let out = output ++ ".part5" in writeFile out part5 >> putStrLn ("Some output written to " ++ out)
