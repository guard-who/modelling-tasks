module Main (main) where

import qualified Data.ByteString                  as BS (writeFile)

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Capabilities.WriteFile.IO        ()
import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.Output
import Modelling.CdOd.Types (
  CdDrawSettings (..),
  ClassDiagram (..),
  Relationship (..),
  defaultOmittedDefaultMultiplicities,
  fromClassDiagram,
  )

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
run withNames howToMark input file = do
  let tokens = lexer input
  let parsed = parser tokens
  output <- drawCd drawSettings howToMark (uncurry toCd parsed)
  BS.writeFile file output
  putStrLn $ "Output written to " ++ file
  where
    toCd cs es = fromClassDiagram ClassDiagram {
      classNames = map fst cs,
      relationships = mapMaybe (uncurry toInheritance) cs ++ es
      }
    toInheritance sub super = Inheritance sub <$> super
    drawSettings = CdDrawSettings {
      omittedDefaults = defaultOmittedDefaultMultiplicities,
      printNames = withNames,
      printNavigations = False
      }

main :: IO ()
main = do
  args <- getArgs
  let (withNames, args') = stripPrintNamesArg args
  case args' of
    [] -> getContents >>= \contents -> run withNames redColor contents "output"
    [file] -> readFile file >>= \contents -> run withNames redColor contents file
    [file, format]
      | map toUpper format == "SVG" ->
          readFile file >>= \contents -> run withNames redColor contents file
      | otherwise -> error $ "format " ++ format
          ++ "is not supported, only SVG is supported"
    [file, format, x]
      | map toUpper format == "SVG" ->
        readFile file >>= \contents ->
          run withNames (specialStyle !! read x) contents file
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
