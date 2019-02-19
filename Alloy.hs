module Alloy where

import Data.List       (intercalate)
import Data.List.Split (splitOn)

import System.FilePath (searchPathSeparator)
import System.IO
import System.Process

getInstances :: Int -> String -> IO [String]
getInstances maxInstances content = do
  let callAlloy = proc "java" ["-cp", '.' : searchPathSeparator : "alloy/Alloy-5.0.0.1.jar",
                               "alloy.RunAlloy", show maxInstances]
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

existInstances :: String -> IO Bool
existInstances = fmap (not . null) . getInstances 1
