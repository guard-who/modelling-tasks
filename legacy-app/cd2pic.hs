module Main (main) where

import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.Output
import Modelling.CdOd.Types             (ClassDiagram (..), Relationship (..))

import Data.Char                        (toUpper)
import Data.GraphViz ()
import Data.Maybe                       (mapMaybe)
import Diagrams.Attributes              (lw, veryThick)
import Diagrams.Prelude                 (Style, (#), red)
import Diagrams.TwoD.Attributes         (lc)
import Diagrams.TwoD.Types              (V2)

import System.Environment (getArgs)

run
  :: Bool
  -> Style V2 Double
  -> String
  -> FilePath
  -> IO ()
run printNames howToMark input file = do
  let tokens = lexer input
  let parsed = parser tokens
  output <- drawCd False printNames howToMark (uncurry toCd parsed) file
  putStrLn $ "Output written to " ++ output
  where
    toCd cs es = ClassDiagram {
      classNames = map fst cs,
      relationships = mapMaybe (uncurry toInheritance) cs ++ es
      }
    toInheritance sub super = Inheritance sub <$> super

main :: IO ()
main = do
  args <- getArgs
  let (printNames, args') = stripPrintNamesArg args
  case args' of
    [] -> getContents >>= \contents -> run printNames redColor contents "output"
    [file] -> readFile file >>= \contents -> run printNames redColor contents file
    [file, format]
      | fmap toUpper format == "SVG" ->
          readFile file >>= \contents -> run printNames redColor contents file
      | otherwise -> error $ "format " ++ format
          ++ "is not supported, only SVG is supported"
    [file, format, x]
      | fmap toUpper format == "SVG" ->
        readFile file >>= \contents ->
          run printNames (specialStyle !! read x) contents file
      | otherwise -> error $ "format " ++ format
          ++ "is not supported, only SVG is supported"
    _ -> error "zu viele Parameter"
  where
    stripPrintNamesArg ("-p":args) = (True, args)
    stripPrintNamesArg args        = (False, args)
    redColor = mempty # lc red

specialStyle :: [Style V2 Double]
specialStyle = map (mempty #) [
  error "not implemented: it should produce a dashed line (maybe using 'dashing'?)",
  error "not implemented: it should produce a dotted line (maybe using 'dashing'?)",
  lw veryThick
  ]
