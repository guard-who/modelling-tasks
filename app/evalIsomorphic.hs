{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.List (nubBy)
import Data.String.Interpolate ( i )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>))

import Modelling.ActivityDiagram.Config (ADConfig(..), adConfigToAlloy, defaultADConfig)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Isomorphism (isADIsomorphic)
import Modelling.ActivityDiagram.PlantUMLConverter (convertToPlantUML)
import Language.Alloy.Call (getInstances)
import Language.PlantUML.Call (DiagramType(SVG), drawPlantUMLDiagram)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToFolder:xs' -> do
      inst <- getInstances (Just 1000) $ adConfigToAlloy "" "" defaultADConfig
      let ad = map (failWith id . parseInstance) inst
          plantumlstring = map convertToPlantUML ad
      writeFile (pathToFolder </> "Stats.txt") $ isomorphismStats ad
      svg <- mapM (drawPlantUMLDiagram SVG) plantumlstring
      writeFilesToSubfolder svg pathToFolder "Debug" "Instance" ".svg"
    _ -> error "usage: one parameter required: FilePath (Output Folder)"

smallConfig :: ADConfig
smallConfig = defaultADConfig {maxActions=4, maxObjectNodes=4, maxNamedNodes=6, decisionMergePairs=1}

largeConfig :: ADConfig
largeConfig = defaultADConfig {minActions=5, maxActions=7, minObjectNodes=5, maxObjectNodes=7, maxNamedNodes=12}

extraLargeConfig :: ADConfig
extraLargeConfig = defaultADConfig {minActions=6, maxActions=8, minObjectNodes=6, maxObjectNodes=8, maxNamedNodes=14, forkJoinPairs=2}

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
