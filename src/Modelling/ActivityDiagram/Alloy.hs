{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Modelling.ActivityDiagram.Alloy(
  moduleComponentsSig,
  moduleInitialNodeRules,
  moduleNameRules,
  moduleReachabilityRules,
  modulePlantUMLSig,
  moduleExerciseRules,
  modulePetrinet,
  moduleActionSequencesRules
) where

import Data.FileEmbed                   (embedStringFile)

moduleComponentsSig :: String
moduleComponentsSig = removeLines 1 $(embedStringFile "alloy/ad/components.als")

moduleInitialNodeRules :: String
moduleInitialNodeRules =
  removeLines 3 $(embedStringFile "alloy/ad/initialNodeRules.als")

moduleNameRules :: String
moduleNameRules = removeLines 3 $(embedStringFile "alloy/ad/nameRules.als")

moduleReachabilityRules :: String
moduleReachabilityRules =
  removeLines 3 $(embedStringFile "alloy/ad/reachabilityRules.als")

modulePlantUMLSig :: String
modulePlantUMLSig = removeLines 3 $(embedStringFile "alloy/ad/plantUml.als")

moduleExerciseRules :: String
moduleExerciseRules =
  removeLines 3 $(embedStringFile "alloy/ad/exerciseRules.als")

modulePetrinet :: String
modulePetrinet = removeLines 3 $(embedStringFile "alloy/ad/petriNet.als")

moduleActionSequencesRules :: String
moduleActionSequencesRules =
  removeLines 3 $(embedStringFile "alloy/ad/actionSequencesRules.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines
