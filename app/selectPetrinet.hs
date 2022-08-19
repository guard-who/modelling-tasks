{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Petrinet (PetriKey(label))
import Modelling.ActivityDiagram.SelectPetri (
  SelectPetriConfig(..),
  SelectPetriInstance(..),
  SelectPetriSolution(..),
  pickRandomLayout, defaultSelectPetriConfig, selectPetriAlloy, checkPetriInstance, selectPetrinet, selectPetriTaskDescription)
import Modelling.ActivityDiagram.PlantUMLConverter(convertToPlantUML)
import Language.Alloy.Call (getInstances)
import Language.PlantUML.Call (DiagramType(SVG), drawPlantUMLDiagram)

import Modelling.PetriNet.Diagram (cacheNet)
import Control.Monad.Except(runExceptT)
import Data.GraphViz.Commands (GraphvizCommand)


main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToFolder:xs' -> do
      let conf = defaultSelectPetriConfig
      inst <- getInstances (Just 50) $ selectPetriAlloy conf
      let ad = map (failWith id . parseInstance "this" "this") inst
          selectPetri =
            map selectPetrinet
            $ filter (isNothing . checkPetriInstance)
            $ map (\x -> SelectPetriInstance{activityDiagram = x, seed=123, numberOfWrongNets=2}) ad
          plantumlstring = map (convertToPlantUML . fst) selectPetri
          taskDescription = replicate (length selectPetri) selectPetriTaskDescription
          taskSolution = map snd selectPetri
      folders <- createExerciseFolders pathToFolder (length selectPetri)
      svg <- mapM (drawPlantUMLDiagram SVG) plantumlstring
      writeFilesToFolders folders B.writeFile svg "Diagram.svg"
      layout <- pickRandomLayout conf
      mapM_ (uncurry (writeSolutionToFolder layout)) $ zip folders taskSolution
      writeFilesToFolders folders writeFile taskDescription  "TaskDescription.txt"
    _ -> error "usage: one parameter required: FilePath (Output Folder)"


writeSolutionToFolder :: GraphvizCommand -> FilePath -> SelectPetriSolution -> IO ()
writeSolutionToFolder layout path SelectPetriSolution {
     matchingNet,
     wrongNets
  } = do
  pathToSolution <- runExceptT $ cacheNet path (show . label) matchingNet False False True layout
  renameFile (failWith id pathToSolution) (path </> "MatchingNet.svg")
  mapM_ (\x -> runExceptT $ cacheNet path (show . label) x False False True layout) wrongNets

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