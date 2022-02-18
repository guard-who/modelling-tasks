module Main (main) where

import Modelling.Auxiliary.Common (upperFirst)
import Modelling.CdOd.Auxiliary.Util (redColor)
import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.Output

import Control.Arrow (first, second)
import Data.GraphViz
import Data.Maybe    (maybeToList)

import System.Environment (getArgs)

run :: Bool -> Attribute -> String -> FilePath -> GraphvizOutput -> IO ()
run printNames howToMark input file format = do
  let tokens = lexer input
  let syntax = parser tokens
  output <- drawCdFromSyntax False printNames (Just howToMark) (first (map $ second maybeToList) syntax) file format
  putStrLn $ "Output written to " ++ output

main :: IO ()
main = do
  args <- getArgs
  let (printNames, args') = stripPrintNamesArg args
  case args' of
    [] -> getContents >>= \contents -> run printNames redColor contents "output" Pdf
    [file] -> readFile file >>= \contents -> run printNames redColor contents file Pdf
    [file, format] -> readFile file >>= \contents -> run printNames redColor contents file (read (upperFirst format))
    [file, format, x] -> readFile file >>= \contents -> run printNames (specialStyle !! read x) contents file (read (upperFirst format))
    _ -> error "zu viele Parameter"
  where
    stripPrintNamesArg ("-p":args) = (True, args)
    stripPrintNamesArg args        = (False, args)

specialStyle :: [Attribute]
specialStyle = map style [dashed, dotted, bold]
