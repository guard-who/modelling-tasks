{-# LANGUAGE QuasiQuotes #-}
-- | Common logic for ActivityDiagram ActionSequences tasks
module Modelling.ActivityDiagram.Auxiliary.ActionSequences (
  actionSequencesAlloy
) where

import Modelling.ActivityDiagram.Alloy (
  adConfigToAlloy,
  moduleActionSequencesRules,
  )
import Modelling.ActivityDiagram.Config (AdConfig)

import Data.String.Interpolate (i)

-- | Common Alloy generation logic for ActionSequences tasks
actionSequencesAlloy
  :: AdConfig
  -> Maybe Bool  -- ^ Whether object nodes should appear on every path
  -> String
actionSequencesAlloy adConfig objectNodeOnEveryPath
  = adConfigToAlloy modules predicates adConfig
  where modules = moduleActionSequencesRules
        predicates =
          [i|
            noActivityFinalNodes
            someActionNodesExistInEachBlock
            #{f objectNodeOnEveryPath "checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            Nothing -> ""
