{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))

import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  adConfigToAlloy,
  defaultAdConfig,
  )
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
      timeOneDefault <- calcTimeForOneInst defaultAdConfig
      timeOneLarge <- calcTimeForOneInst largeConfig
      timeOneExtraLarge <- calcTimeForOneInst extraLargeConfig
      writeFile (pathToFolder </> "Stats.txt") $ timeStats timeOneSmall timeOneDefault timeOneLarge timeOneExtraLarge
    _ -> error "usage: one parameter required: FilePath (Output Folder)"


calcTimeForOneInst :: AdConfig -> IO String
calcTimeForOneInst conf = do
  start <- getTime
  _ <- getInstances (Just 1) $ adConfigToAlloy "" "" conf
  end <- getTime
  let !delta = end - start
  return $ secs delta

smallConfig :: AdConfig
smallConfig = defaultAdConfig {
  objectNodeLimits = (1, 4),
  maxNamedNodes = 6,
  decisionMergePairs = 1
  }

largeConfig :: AdConfig
largeConfig = defaultAdConfig {
  actionLimits = (5, 7),
  objectNodeLimits = (5, 7),
  maxNamedNodes = 12}

extraLargeConfig :: AdConfig
extraLargeConfig = defaultAdConfig {
  actionLimits = (6, 8),
  objectNodeLimits = (6, 8),
  maxNamedNodes = 14,
  forkJoinPairs = 2
  }

timeStats :: String -> String -> String -> String -> String
timeStats timeOneSmall timeOneDefault timeOneLarge timeOneExtraLarge =
  [i|
    Time for one instance (Small Config): #{timeOneSmall}
    Time for one instance (Default Config): #{timeOneDefault}
    Time for one instance (Large Config): #{timeOneLarge}
    Time for one instance (Extra Large Config): #{timeOneExtraLarge}
  |]
