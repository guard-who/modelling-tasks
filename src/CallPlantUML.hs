module CallPlantUML (
  processPlantUMLString
) where

import qualified Data.ByteString as BS
  (hGetContents)

import Data.ByteString (ByteString)

import System.IO
  (hClose, hFlush, hPutStr)

import System.Process (
    CreateProcess(..), StdStream (..),
    cleanupProcess, createProcess, proc
  )

processPlantUMLString :: String -> String -> IO ByteString
processPlantUMLString plantumlstring pathToJar = do  
  let callPlantUML = proc "java" $ ["-jar", pathToJar, "-p"] ++ ["-svg"]
  (Just hin, Just hout, Just herr, ph) <-
    createProcess callPlantUML {
        std_out = CreatePipe,
        std_in  = CreatePipe,
        std_err = CreatePipe
      }
  hPutStr hin plantumlstring
  hFlush hin
  hClose hin
  out <- BS.hGetContents hout
  cleanupProcess (Just hin, Just hout, Just herr, ph)
  return out

