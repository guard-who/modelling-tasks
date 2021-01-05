{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Alloy (
  compAdvConstraints, compBasicConstraints, compChange,
  connected,
  getAlloyInstances,
  isolated,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriNetRnd, petriScopeBitwidth, petriScopeMaxSeq, renderFalse,
  ) where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
  )

import Modelling.PetriNet.Types

import Control.Monad                    (when)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
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

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "lib/Alloy/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "lib/Alloy/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "lib/Alloy/Helpers.als")

modulePetriConcepts :: String
modulePetriConcepts = removeLines 5 $(embedStringFile "lib/Alloy/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 5 $(embedStringFile "lib/Alloy/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd basicC@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets[#{activated} : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints activated basicC}
  #{compAdvConstraints advConfig}
}
run showNets for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated = "activatedTrans"

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

pred showFalseNets[#{activated} : set Transitions]{
  #{compBasicConstraints activated basicTask}
  #{compAdvConstraints advTask}
  #{compChange changeTask}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int
|]
  where
    (ps, ts)    = M.partition isPlaceNode allNodes
    activated   = "activatedTrans"
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

{-|
A set of constraints enforcing settings of 'BasicConfig'.
-}
compBasicConstraints
  :: String
  -- ^ The name of the Alloy variable for the set of activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
compBasicConstraints activatedTrans BasicConfig {
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
  ##{activatedTrans} >= #{atLeastActive}
  theActivatedTransitions[#{activatedTrans}]
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
  where
    petriLoops = \case
      True  -> "some n : Nodes | selfLoop[n]"
      False -> "no n : Nodes | selfLoop[n]"
    petriSink = \case
      True  -> "some t : Transitions | sinkTransitions[t]"
      False -> "no t : Transitions | sinkTransitions[t]"
    petriSource = \case
      True  -> "some t : Transitions | sourceTransitions[t]"
      False -> "no t : Transitions | sourceTransitions[t]"

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

getAlloyInstances
  :: CallAlloyConfig
  -> String
  -> ExceptT String IO [AlloyInstance]
getAlloyInstances config alloy = do
  list <- lift $ getInstancesWith config alloy
  when (null list) $ except $ Left "no instance available"
  return list
