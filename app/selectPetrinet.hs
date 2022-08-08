{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import Modelling.ActivityDiagram.Alloy (getRawAlloyInstancesWith)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Petrinet (PetriKey(label))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriInstance(..), SelectPetriSolution(..), defaultSelectPetriConfig, selectPetriAlloy, selectPetrinet, selectPetriTaskDescription)
import Modelling.ActivityDiagram.PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

import Modelling.PetriNet.Diagram (cacheNet)
import Control.Monad.Except(runExceptT)
import Data.GraphViz.Commands (GraphvizCommand(..))


main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstancesWith (Just 50) $ selectPetriAlloy defaultSelectPetriConfig
      writeFilesToSubfolder inst pathToFolder "Debug" "Exercise" ".als"
      folders <- createExerciseFolders pathToFolder (length inst)
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          selectPetri = map (\x ->SelectPetriInstance{activityDiagram = x, seed=123, numberOfWrongNets=2}) ad
          plantumlstring = map convertToPlantUML ad
          taskDescription = replicate (length folders) selectPetriTaskDescription
          taskSolution = map selectPetrinet selectPetri
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToFolders folders B.writeFile svg "Diagram.svg"
      mapM_ (uncurry writeSolutionToFolder) $ zip folders taskSolution
      writeFilesToFolders folders writeFile taskDescription  "TaskDescription.txt"
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"


writeSolutionToFolder :: FilePath -> SelectPetriSolution -> IO ()
writeSolutionToFolder path SelectPetriSolution {
     matchingNet,
     wrongNets
  } = do
  pathToSolution <- runExceptT $ cacheNet path (show.label) matchingNet False False True Dot
  renameFile (failWith id pathToSolution) (path </> "MatchingNet.svg")
  mapM_ (\x -> runExceptT $ cacheNet path (show . label) x False False True Dot) wrongNets

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