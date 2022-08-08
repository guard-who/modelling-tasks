{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Modelling.ActivityDiagram.Alloy(
  getAlloyInstances,
  getAlloyInstancesWith,
  getRawAlloyInstances,
  getRawAlloyInstancesWith,
  moduleComponentsSig,
  moduleInitialNodeRules,
  moduleNameRules,
  moduleReachabilityRules,
  modulePlantUMLSig,
  moduleExerciseRules,
  modulePetrinet,
  moduleActionSequencesRules
) where

import qualified Data.ByteString as B (split, intercalate)

import Data.ByteString (ByteString)
import Data.FileEmbed                   (embedStringFile)
import Data.List                        (intercalate)

import Language.Alloy.Call (
  AlloyInstance
  )

import Language.Alloy.Debug (
  parseInstance,
  getRawInstances
  )

moduleComponentsSig :: String
moduleComponentsSig = removeLines 1 $(embedStringFile "alloy/ad/ad_components_sig.als")

moduleInitialNodeRules :: String
moduleInitialNodeRules = removeLines 3 $(embedStringFile "alloy/ad/ad_initialnode_rules.als")

moduleNameRules :: String
moduleNameRules = removeLines 3 $(embedStringFile "alloy/ad/ad_name_rules.als")

moduleReachabilityRules :: String
moduleReachabilityRules = removeLines 3 $(embedStringFile "alloy/ad/ad_reachability_rules.als")

modulePlantUMLSig :: String
modulePlantUMLSig = removeLines 3 $(embedStringFile "alloy/ad/ad_plantuml_sig.als")

moduleExerciseRules :: String
moduleExerciseRules = removeLines 3 $(embedStringFile "alloy/ad/ad_exercise_rules.als")

modulePetrinet :: String
modulePetrinet = removeLines 3 $(embedStringFile "alloy/ad/ad_petrinet.als")

moduleActionSequencesRules :: String
moduleActionSequencesRules = removeLines 3 $(embedStringFile "alloy/ad/ad_actionsequences_rules.als")

completeSpec :: String
completeSpec = intercalate "\n" [moduleComponentsSig, moduleInitialNodeRules, moduleNameRules, moduleReachabilityRules, modulePlantUMLSig, moduleExerciseRules]

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

--For now just with static scope from file
getAlloyInstances :: Maybe Integer -> IO [AlloyInstance]
getAlloyInstances n = getAlloyInstancesWith n completeSpec

--For debugging
getRawAlloyInstances :: Maybe Integer -> IO [ByteString]
getRawAlloyInstances n = getRawAlloyInstancesWith n completeSpec

getAlloyInstancesWith :: Maybe Integer -> String -> IO [AlloyInstance]
getAlloyInstancesWith n spec =
  map (either (error . show) id . parseInstance) <$>
  getRawAlloyInstancesWith n spec

getRawAlloyInstancesWith :: Maybe Integer -> String -> IO [ByteString]
getRawAlloyInstancesWith n spec = preprocess <$> getRawInstances n spec

--Remove problematic line from getRawInstances output
preprocess :: [ByteString] -> [ByteString]
preprocess = map preprocess'

preprocess' :: ByteString -> ByteString
preprocess' s = let linesOfByteString = B.split 10 s
                in B.intercalate "\n" $ filter (/= "------State 0-------") linesOfByteString