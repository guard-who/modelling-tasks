module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import Modelling.ActivityDiagram.Alloy (getRawAlloyInstancesWith)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.SelectAS (SelectASInstance(..), defaultSelectASConfig, selectASAlloy, checkSelectASInstance, selectActionSequenceText, selectASTaskDescription)
import Modelling.ActivityDiagram.PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstancesWith (Just 50) $ selectASAlloy defaultSelectASConfig
      writeFilesToSubfolder inst pathToFolder "Debug" "Exercise" ".als"
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          selectAS = filter (isNothing . (`checkSelectASInstance` defaultSelectASConfig))
                      $ map (\x -> SelectASInstance{activityDiagram = x, seed=123, numberOfWrongSequences=2}) ad
          plantumlstring = map (convertToPlantUML . activityDiagram) selectAS
          taskDescription = map selectASTaskDescription selectAS
          taskSolution = map selectActionSequenceText selectAS
      folders <- createExerciseFolders pathToFolder (length selectAS)
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToFolders folders B.writeFile svg "Diagram.svg"
      writeFilesToFolders folders writeFile taskDescription  "TaskDescription.txt"
      writeFilesToFolders folders writeFile taskSolution "TaskSolution.txt"
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

createExerciseFolders :: FilePath -> Int -> IO [FilePath]
createExerciseFolders path n = do
  let pathToFolders = map (\x -> path </> ("Exercise" ++ show x)) [1..n]
  mapM_ (createDirectoryIfMissing True) pathToFolders
  return $ map addTrailingPathSeparator pathToFolders

writeFilesToFolders :: [FilePath] -> (FilePath -> a -> IO()) -> [a] -> String -> IO ()
writeFilesToFolders folders writeFn files filename = do
  let paths = map (</> filename) folders
  mapM_ (uncurry writeFn) $ zip paths files

writeFilesToSubfolder :: [ByteString] -> FilePath -> FilePath -> String -> String -> IO ()
writeFilesToSubfolder files path subfolder prefix extension = do
  let pathToFolder = path </> subfolder
  createDirectoryIfMissing True pathToFolder
  mapM_ (\(x,y) -> B.writeFile (pathToFolder </> (prefix ++ show x ++ extension)) y) $ zip [1..] files