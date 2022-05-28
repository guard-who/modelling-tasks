module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>))


import AD_Alloy (getRawAlloyInstances)
import AD_Instance (parseInstance)
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstances (Just 50)
      writeFilesToSubfolder inst pathToFolder "AlloyInstances" "Diagram" ".als"
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          plantumlstring = map convertToPlantUML ad
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToSubfolder svg pathToFolder "Diagrams" "Diagram" ".svg"
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

writeFilesToSubfolder :: [ByteString] -> FilePath -> FilePath -> String -> String -> IO ()
writeFilesToSubfolder files path subfolder prefix extension = do
  let pathToFolder = path </> subfolder
  createDirectoryIfMissing True pathToFolder
  mapM_ (\(x,y) -> B.writeFile (pathToFolder </> (prefix ++ show x ++ extension)) y) $ zip [1..] files
