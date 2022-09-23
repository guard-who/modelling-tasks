{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))

import Modelling.ActivityDiagram.Config (ADConfig(..), adConfigToAlloy, defaultADConfig)
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import Criterion.Measurement (secs, initializeTime, getTime)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [pathToFolder] -> do
      initializeTime
      timeOneSmall <- calcTimeForOneInst smallConfig
      timeOneDefault <- calcTimeForOneInst defaultADConfig
      timeOneLarge <- calcTimeForOneInst largeConfig
      timeOneExtraLarge <- calcTimeForOneInst extraLargeConfig
      writeFile (pathToFolder </> "Stats.txt") $ timeStats timeOneSmall timeOneDefault timeOneLarge timeOneExtraLarge
    _ -> error "usage: one parameter required: FilePath (Output Folder)"


calcTimeForOneInst :: ADConfig -> IO String
calcTimeForOneInst conf = do
  start <- getTime
  _ <- getInstances (Just 1) $ adConfigToAlloy "" "" conf
  end <- getTime
  let !delta = end - start
  return $ secs delta

smallConfig :: ADConfig
smallConfig = defaultADConfig {maxActions=4, maxObjectNodes=4, maxNamedNodes=6, decisionMergePairs=1}

largeConfig :: ADConfig
largeConfig = defaultADConfig {minActions=5, maxActions=7, minObjectNodes=5, maxObjectNodes=7, maxNamedNodes=12}

extraLargeConfig :: ADConfig
extraLargeConfig = defaultADConfig {minActions=6, maxActions=8, minObjectNodes=6, maxObjectNodes=8, maxNamedNodes=14, forkJoinPairs=2}

timeStats :: String -> String -> String -> String -> String
timeStats timeOneSmall timeOneDefault timeOneLarge timeOneExtraLarge =
  [i|
    Time for one instance (Small Config): #{timeOneSmall}
    Time for one instance (Default Config): #{timeOneDefault}
    Time for one instance (Large Config): #{timeOneLarge}
    Time for one instance (Extra Large Config): #{timeOneExtraLarge}
  |]
