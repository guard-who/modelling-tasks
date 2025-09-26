module Main (main) where
import qualified Data.ByteString.Char8            as BS (pack)

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Capabilities.WriteFile.IO        ()
import Modelling.CdOd.Output            (drawOdFromInstance)

import Control.Monad (void)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Char                        (toUpper)
import Data.GraphViz                    (DirType (NoDir))
import Data.Ratio                       ((%))

import System.Environment (getArgs)
import Language.Alloy.Debug             (parseInstance)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> error "possible links required (first parameter)"
   [xs] -> getContents >>= drawOd (read xs) "output"
   [xs, file] -> readFile file >>= drawOd (read xs) file
   [xs, file, format]
     | map toUpper format == "SVG" -> readFile file >>= drawOd (read xs) file
     | otherwise -> error $ "format " ++ format
         ++ "is not supported, only SVG is supported"
   _ -> error "zu viele Parameter"

drawOd :: [String] -> FilePath -> String -> IO ()
drawOd possibleLinks file contents = flip evalRandT (mkStdGen 0) $ do
  i <- parseInstance $ BS.pack contents
  output <- drawOdFromInstance
    i
    Nothing
    possibleLinks
    (Just $ 1 % 3)
    NoDir
    False
    (file ++ ".svg")
  lift . putStrLn $ "Output written to " ++ output
