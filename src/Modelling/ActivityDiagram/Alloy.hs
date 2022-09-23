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

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines
