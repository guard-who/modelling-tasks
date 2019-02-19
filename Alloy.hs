module Alloy where

import Data.List       (intercalate)
import Data.List.Split (splitOn)

import System.FilePath (searchPathSeparator)
import System.IO
import System.Process

getInstances :: Int -> String -> IO [String]
getInstances maxInsta content = do
  let callAlloy = proc "java" ["-cp", '.' : searchPathSeparator : "alloy/Alloy-5.0.0.1.jar",
                               "alloy.RunAlloy", show maxInsta]
  (Just hin, Just hout, _, _) <- createProcess callAlloy { std_out = CreatePipe, std_in = CreatePipe }
  hPutStr hin content
  hClose hin
  fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  where
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> hGetLine h <*> getWholeOutput h
