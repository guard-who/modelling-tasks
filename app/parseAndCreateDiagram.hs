module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (readFile, writeFile)

import System.Environment (getArgs, withArgs)

import AD_Instance (parseInstance)
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    scope:pathToJar:pathToSvg:f:xs' -> do
      inst <- B.readFile f
      let ad = failWith id . parseInstance scope scope . failWith show
            $ AD.parseInstance inst
          plantumlstring = convertToPlantUML ad
      svg <- processPlantUMLString plantumlstring pathToJar
      B.writeFile pathToSvg svg
    _ -> error "usage: three parameters required: String (scope) FilePath (PlantUML jar) FilePath (Output File) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id