{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.List (nubBy)
import Data.String.Interpolate ( i )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>))

import AD_Alloy (getRawAlloyInstancesWith)
import AD_Config (adConfigToAlloy, defaultADConfig)
import AD_Datatype (UMLActivityDiagram)
import AD_Instance (parseInstance)
import AD_Isomorphism (isADIsomorphic)
import AD_PlantUMLConverter (convertToPlantUML)
import CallPlantUML (processPlantUMLString)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstancesWith Nothing $ adConfigToAlloy "" "" defaultADConfig
      writeFilesToSubfolder inst pathToFolder "Debug" "Instance" ".als"
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          plantumlstring = map convertToPlantUML ad
      writeFile (pathToFolder </> "Stats.txt") $ isomorphismStats ad
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToSubfolder svg pathToFolder "Debug" "Instance" ".svg"
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

isomorphismStats :: [UMLActivityDiagram] -> String
isomorphismStats xs =
  [i|
    Number of generated instances: #{numberOfInst}
    Number of non-isomorphic instances: #{numberOfDiffInst}
    Different diagrams per Generation in Percent: #{percentage numberOfDiffInst numberOfInst}
  |]
  where
    numberOfInst = length xs
    numberOfDiffInst = length $ nubBy isADIsomorphic xs

percentage :: (Integral a) => a -> a -> Double
percentage x y = fromIntegral x / fromIntegral y * 100.0

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

writeFilesToSubfolder :: [ByteString] -> FilePath -> FilePath -> String -> String -> IO ()
writeFilesToSubfolder files path subfolder prefix extension = do
  let pathToFolder = path </> subfolder
  createDirectoryIfMissing True pathToFolder
  mapM_ (\(x,y) -> B.writeFile (pathToFolder </> (prefix ++ show x ++ extension)) y) $ zip [1..] files
