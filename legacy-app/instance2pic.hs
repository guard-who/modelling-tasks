module Main (main) where
import qualified Data.ByteString.Char8            as BS (pack)

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.Output            (drawOdFromInstance)

import Control.Monad (void)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Char                        (toUpper)
import Data.GraphViz                    (DirType (NoDir))
import Data.List                        (isPrefixOf, stripPrefix)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromJust)

import System.Environment (getArgs)
import Language.Alloy.Debug             (parseInstance)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> getContents >>= drawOd "output"
   [file] -> readFile file >>= drawOd file
   [file, format]
     | fmap toUpper format == "SVG" -> readFile file >>= drawOd file
     | otherwise -> error $ "format " ++ format
         ++ "is not supported, only SVG is supported"
   _ -> error "zu viele Parameter"

drawOd :: FilePath -> String -> IO ()
drawOd file contents = flip evalRandT (mkStdGen 0) $ do
  let [objLine, _] = filter ("this/Object" `isPrefixOf`) (lines contents)
      theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Object=" objLine))))
  i <- parseInstance $ BS.pack contents
  output <- drawOdFromInstance
    i
    (Just $ length theNodes `div` 3)
    NoDir
    False
    (file ++ ".svg")
  lift . putStrLn $ "Output written to " ++ output
