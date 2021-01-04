{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Alloy (
  concurrencyTransition1, concurrencyTransition2,
  conflictPlaces1, conflictTransition1, conflictTransition2,
  getAlloyInstances,
  petriNetFindConcur, petriNetFindConfl, petriNetPickConcur, petriNetPickConfl,
  petriNetRnd, petriScopeBitwidth, petriScopeMaxSeq, renderFalse,
  ) where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
  )

import Modelling.PetriNet.Types

import Control.Monad                    (when)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.Maybe                       (isJust, isNothing)
import Data.FileEmbed
import Data.String.Interpolate
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig, getInstancesWith,
  )

petriScopeBitwidth :: BasicConfig -> Int
petriScopeBitwidth BasicConfig
 {places,transitions,maxFlowOverall,maxTokensOverall} = 
  floor 
     (2 + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) 
       (maximum [maxFlowOverall,maxTokensOverall,places,transitions])
     )
  
petriScopeMaxSeq :: BasicConfig -> Int
petriScopeMaxSeq BasicConfig{places,transitions} = places+transitions
  
petriLoops :: Bool -> String
petriLoops = \case
 True  -> "some n : Nodes | selfLoop[n]"
 False -> "no n : Nodes | selfLoop[n]"

petriSink :: Bool -> String
petriSink = \case
 True  -> "some t : Transitions | sinkTransitions[t]"
 False -> "no t : Transitions | sinkTransitions[t]"

petriSource :: Bool -> String
petriSource = \case
 True  -> "some t : Transitions | sourceTransitions[t]"
 False -> "no t : Transitions | sourceTransitions[t]"

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "lib/Alloy/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "lib/Alloy/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "lib/Alloy/Helpers.als")

modulePetriConcepts :: String 
modulePetriConcepts = removeLines 5 $(embedStringFile "lib/Alloy/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 4 $(embedStringFile "lib/Alloy/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd input@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [activatedTrans : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints input}
  #{compAdvConstraints advConfig}
  
}
run showNets for exactly #{petriScopeMaxSeq input} Nodes, #{petriScopeBitwidth input} Int

|]

renderFalse :: PetriLike String -> MathConfig -> String
renderFalse
  PetriLike  {allNodes}
  MathConfig {basicTask, advTask, changeTask} = [i|module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{places}
#{transitions}

fact{
#{initialMark}

#{defaultFlow}
}

pred showFalseNets[activatedTrans : set Transitions]{
  #{compBasicConstraints basicTask}
  #{compAdvConstraints advTask}
  #{compChange changeTask}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int
|]
  where
    (ps, ts)    = M.partition isPlaceNode allNodes
    places      = unlines [extendLine p "givenPlaces" | p <- M.keys ps]
    transitions = unlines [extendLine t "givenTransitions" | t <- M.keys ts]
    initialMark = M.foldrWithKey (\k -> (++) . tokenLine k) "" $ initial <$> ps
    defaultFlow = M.foldrWithKey (\k _ -> (printFlow k ++)) "" allNodes
    printFlow :: String -> String
    printFlow x = M.foldrWithKey
      (\y flow -> (++) $ flowLine x y $ M.lookup x $ flowIn flow)
      ""
      allNodes

extendLine :: String -> String -> String
extendLine n k = [i|one sig #{n} extends #{k}{}
|]

tokenLine :: String -> Int -> String
tokenLine k l = [i|  #{k}.defaultTokens = #{l}
|]

flowLine :: String -> String -> Maybe Int -> String
flowLine from to Nothing  = [i|  no #{from}.defaultFlow[#{to}]
|]
flowLine from to (Just f) = [i|  #{from}.defaultFlow[#{to}] = #{f}
|]

conflictPredicateName :: String
conflictPredicateName = "showConflict"

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig {
  basicConfig,
  advConfig,
  changeConfig,
  uniqueConflictPlace
  } = petriNetAlloy basicConfig changeConfig (Just uniqueConflictPlace) $ Just advConfig

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl PickConflictConfig {
  basicConfig,
  changeConfig,
  uniqueConflictPlace
  } = petriNetAlloy basicConfig changeConfig (Just uniqueConflictPlace) Nothing

concurrencyPredicateName :: String
concurrencyPredicateName = "showConcurrency"

petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{
  basicConfig,
  advConfig,
  changeConfig
  } = petriNetAlloy basicConfig changeConfig Nothing $ Just advConfig

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur PickConcurrencyConfig{
  basicConfig,
  changeConfig
  } = petriNetAlloy basicConfig changeConfig Nothing Nothing

{-|
Generate code for PetriNet conflict and concurrency tasks
-}
petriNetAlloy
  :: BasicConfig
  -> ChangeConfig
  -> Maybe (Maybe Bool)
  -- ^ Just for conflict task; Nothing for concurrency task
  -> Maybe AdvConfig
  -- ^ Just for find task; Nothing for pick task
  -> String
petriNetAlloy basicC changeC muniquePlace specific
  = [i|module #{moduleName}

#{modulePetriSignature}
#{maybe "" (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{predicate}[#{place}#{defaultActivTrans}activatedTrans : set Transitions, #{t1}, #{t2} : Transitions] {
  #Places = #{places basicC}
  #Transitions = #{transitions basicC}
  #{compBasicConstraints basicC}
  #{compChange changeC}
  #{maybe "" multiplePlaces muniquePlace}
  #{constraints}
  #{compConstraints}
}

run #{predicate} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    compConstraints = case specific of
      Just advC ->
        compAdvConstraints advC
      Nothing -> [i|
  #{connected "defaultGraphIsConnected" $ isConnected basicC}
  #{isolated "defaultNoIsolatedNodes" $ isConnected basicC}
  #defaultActivTrans >= #{atLeastActive basicC}
  theActivatedDefaultTransitions[defaultActivTrans]|]
    conflict = isJust muniquePlace
    constraints :: String
    constraints
      | conflict  = [i|
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  all q : #{p} | conflict[#{t1}, #{t2}, q]
  no q : (Places - #{p}) | conflict[#{t1}, #{t2}, q]
  all u,v : Transitions, q : Places |
    conflict[u,v,q] implies #{t1} + #{t2} = u + v|]
      | otherwise = [i|
  no x,y : givenTransitions | concurrentDefault[x+y] and x != y
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
    and all u,v : Transitions |
      concurrent[u + v] and u != v implies #{t1} + #{t2} = u + v|]
    defaultActivTrans
      | isNothing specific = "defaultActivTrans : set givenTransitions,"
      | otherwise          = ""
    moduleName
      | conflict  = "PetriNetConfl"
      | otherwise = "PetriNetConcur"
    multiplePlaces unique
      | unique == Just True
      = [i|one #{p}|]
      | unique == Just False
      = [i|not (one #{p})|]
      | otherwise
      = ""
    p  = places1
    place
      | conflict  = [i|#{p} : some Places,|]
      | otherwise = ""
    predicate
      | conflict  = conflictPredicateName
      | otherwise = concurrencyPredicateName
    t1 = transition1
    t2 = transition2

----------------------"Building-Kit"----------------------------

-- Needs: activatedTrans : set Transitions
compBasicConstraints :: BasicConfig -> String
compBasicConstraints BasicConfig {
  atLeastActive,
  isConnected,
  maxFlowOverall,
  maxFlowPerEdge,
  maxTokensOverall,
  maxTokensPerPlace,
  minFlowOverall,
  minTokensOverall
  } = [i|
  let t = (sum p : Places | p.tokens) | t >= #{minTokensOverall} and t <= #{maxTokensOverall}
  all p : Places | p.tokens =< #{maxTokensPerPlace}
  all weight : Nodes.flow[Nodes] | weight =< #{maxFlowPerEdge}
  let theflow = (sum f, t : Nodes | f.flow[t]) |
    theflow >= #{minFlowOverall} and #{maxFlowOverall} >= theflow
  #activatedTrans >= #{atLeastActive}
  theActivatedTransitions[activatedTrans]
  #{connected "graphIsConnected" isConnected}
  #{isolated "noIsolatedNodes" isConnected}
|]

connected :: String -> Maybe Bool -> String
connected p = maybe "" $ \c -> (if c then "" else "not ") ++ p

isolated :: String -> Maybe Bool -> String
isolated p = maybe p $ \c -> if c then "" else p

compAdvConstraints :: AdvConfig -> String
compAdvConstraints AdvConfig
                        { presenceOfSelfLoops, presenceOfSinkTransitions
                        , presenceOfSourceTransitions
                        } = [i| 
  #{maybe "" petriLoops presenceOfSelfLoops}
  #{maybe "" petriSink presenceOfSinkTransitions}
  #{maybe "" petriSource presenceOfSourceTransitions}
|]

compChange :: ChangeConfig -> String
compChange ChangeConfig
                  {flowChangeOverall, maxFlowChangePerEdge
                  , tokenChangeOverall, maxTokenChangePerPlace
                  } = [i|
  #{flowChangeOverall} = (sum f, t : Nodes | abs[f.flowChange[t]])
  maxFlowChangePerEdge [#{maxFlowChangePerEdge}]
  #{tokenChangeOverall} = (sum p : Places | abs[p.tokenChange])
  maxTokenChangePerPlace [#{maxTokenChangePerPlace}]
|]

skolemVariable :: String -> String -> String
skolemVariable x y = '$' : x ++ '_' : y

concurrencyTransition1 :: String
concurrencyTransition1 = skolemVariable concurrencyPredicateName transition1

concurrencyTransition2 :: String
concurrencyTransition2 = skolemVariable concurrencyPredicateName transition2

conflictPlaces1 :: String
conflictPlaces1 = skolemVariable conflictPredicateName places1

conflictTransition1 :: String
conflictTransition1 = skolemVariable conflictPredicateName transition1

conflictTransition2 :: String
conflictTransition2 = skolemVariable conflictPredicateName transition2

transition1 :: String
transition1 = "transition1"

transition2 :: String
transition2 = "transition2"

places1 :: String
places1 = "places"

getAlloyInstances
  :: CallAlloyConfig
  -> String
  -> ExceptT String IO [AlloyInstance]
getAlloyInstances config alloy = do
  list <- lift $ getInstancesWith config alloy
  when (null list) $ except $ Left "no instance available"
  return list
