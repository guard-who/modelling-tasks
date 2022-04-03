module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (readFile)

import System.Environment (getArgs, withArgs)
import Text.Pretty.Simple (pPrint)

import AD_Instance (parseInstance)


main :: IO ()
main = do
  xs <- getArgs
  case xs of
    scope:f:xs' -> do
      inst <- B.readFile f
      let ad = failWith id . parseInstance scope . failWith show
            $ AD.parseInstance inst
      pPrint ad
    _ -> error "usage: two parameters required: String (scope) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id