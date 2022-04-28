module Main where

import qualified Data.ByteString as B (writeFile)

import System.Environment (getArgs, withArgs)
import Control.Monad(sequence)


import AD_Alloy (getAlloyInstances)
import AD_Instance (parseInstance)
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getAlloyInstances (Just 50) 
      let ad = map (failWith id . parseInstance "this" "this") inst
          plantumlstring = map convertToPlantUML ad
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      mapM_ (\(x,y) -> B.writeFile (pathToFolder ++ "Diagram" ++ show x ++ ".svg") y) $ zip [1..] svg
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id