module Main (main) where

import Auxiliary.Util
import Auxiliary.Lexer (lexer)
import Auxiliary.Parser (parser)
import Output

import Control.Arrow (first, second)
import Data.GraphViz
import Data.Maybe    (maybeToList)

import System.Environment (getArgs)

run :: Bool -> Attribute -> String -> FilePath -> GraphvizOutput -> IO ()
run printNames howToMark input = do
  let tokens = lexer input
  let syntax = parser tokens
  drawCdFromSyntax False printNames (Just howToMark) $ first (second maybeToList <$>) syntax

main :: IO ()
main = do
  args <- getArgs
  let (printNames, args') = stripPrintNamesArg args
  case args' of
    [] -> getContents >>= \contents -> run printNames redColor contents "output" Pdf
    [file] -> readFile file >>= \contents -> run printNames redColor contents file Pdf
    [file, format] -> readFile file >>= \contents -> run printNames redColor contents file (read (firstUpper format))
    [file, format, x] -> readFile file >>= \contents -> run printNames (specialStyle !! read x) contents file (read (firstUpper format))
    _ -> error "zu viele Parameter"
  where
    stripPrintNamesArg ("-p":args) = (True, args)
    stripPrintNamesArg args        = (False, args)

specialStyle :: [Attribute]
specialStyle = map style [dashed, dotted, bold]
