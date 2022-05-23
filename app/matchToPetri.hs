module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy(toStrict)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import AD_Alloy (getRawAlloyInstancesWith)
import AD_Instance (parseInstance)
import AD_MatchComponents(matchPetriComponents, defaultMatchPetriConfig, matchPetriAlloy)
import AD_Petrinet (convertToPetrinet, PetriKey(..))
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

import Modelling.PetriNet.Diagram (cacheNet)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Control.Monad.Except(runExceptT)

import Data.Aeson(encode)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstancesWith (Just 50) $ matchPetriAlloy defaultMatchPetriConfig
      folders <- createExerciseFolders pathToFolder (length inst)
      writeFilesToFolders folders inst "Diagram.als"
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          plantumlstring = map convertToPlantUML ad
          petri = map convertToPetrinet ad
          json =  zipWith (\x y -> toStrict $ encode $ matchPetriComponents x y) ad petri
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToFolders folders svg "Diagram.svg"
      mapM_ (\(x,y) -> runExceptT $ cacheNet x (show . label) y False False True Dot) $ zip folders petri
      writeFilesToFolders folders json "MatchExercise.json"
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

createExerciseFolders :: FilePath -> Int -> IO [FilePath]
createExerciseFolders path n = do
  let pathToFolders = map (\x -> path </> ("Exercise" ++ show x)) [1..n]
  mapM_ (createDirectoryIfMissing True) pathToFolders
  return $ map addTrailingPathSeparator pathToFolders

writeFilesToFolders :: [FilePath] -> [ByteString] -> String -> IO ()  
writeFilesToFolders folders files filename = do
  let paths = map (</> filename) folders
  mapM_ (uncurry B.writeFile) $ zip paths files