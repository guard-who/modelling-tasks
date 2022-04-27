{-# LANGUAGE TemplateHaskell #-}

module AD_Alloy(
  getAlloyInstances
) where

import Data.FileEmbed                   (embedStringFile)
import Data.List                        (intercalate)

import Language.Alloy.Call (
  AlloyInstance,
  getInstances
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

completeSpec :: String 
completeSpec = intercalate "\n" [moduleComponentsSig, moduleInitialNodeRules, moduleNameRules, moduleReachabilityRules, modulePlantUMLSig]

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

--For now just with static scope from file
getAlloyInstances :: Maybe Integer -> IO [AlloyInstance]
getAlloyInstances n = getInstances n completeSpec