module Main where

import qualified Data.ByteString as B (writeFile)

import System.Environment (getArgs, withArgs)

import AD_Alloy (getAlloyInstances)
import AD_Instance (parseInstance)
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToSvg:xs' -> do
      inst <- getAlloyInstances (Just 1)
      let ad = failWith id . parseInstance "this" "this" $ head inst
          plantumlstring = convertToPlantUML ad
      svg <- processPlantUMLString plantumlstring pathToJar
      B.writeFile pathToSvg svg
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output File)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id