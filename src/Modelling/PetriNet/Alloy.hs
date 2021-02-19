{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# Language DuplicateRecordFields #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  connected,
  getAlloyInstances,
  isolated,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitwidth,
  petriScopeMaxSeq,
  taskInstance,
  ) where

import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig,
  BasicConfig (..),
  ChangeConfig (..),
  )

import qualified Modelling.PetriNet.Types         as T (
  AlloyConfig (maxInstances, timeout)
  )

import Control.Monad                    (when)
import Control.Monad.Random (
  RandT,
  Random (randomR),
  RandomGen,
  liftRandT,
  )
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.FileEmbed                   (embedStringFile)
import Data.GraphViz                    (GraphvizCommand)
import Data.Maybe                       (isNothing)
import Data.String.Interpolate          (i)
import Language.Alloy.Call (
  AlloyInstance,
  CallAlloyConfig (maxInstances, timeout),
  defaultCallAlloyConfig,
  getInstancesWith,
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
modulePetriSignature = removeLines 2 $(embedStringFile "alloy/petri/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "alloy/petri/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "alloy/petri/Helpers.als")

modulePetriConcepts :: String
modulePetriConcepts = removeLines 5 $(embedStringFile "alloy/petri/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 5 $(embedStringFile "alloy/petri/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

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
  \##{activatedTrans} >= #{atLeastActive}
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

taskInstance
  :: RandomGen g
  => (f -> AlloyInstance -> GraphvizCommand -> ExceptT String IO a)
  -> (config -> String)
  -> f
  -> (config -> GraphvizCommand)
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> RandT g (ExceptT String IO) a
taskInstance taskF alloyF parseF layoutF alloyC config segment = do
  let is = fromIntegral <$> T.maxInstances (alloyC config)
  x <- sequence $ randomInSegment segment <$> is
  list <- lift $ getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = toInteger . (+1) <$> x,
      timeout      = T.timeout (alloyC config)
      }
    (alloyF config)
  when (null list) $ lift $ except $ Left "no instance available"
  when (null $ drop segment list)
    $ lift $ except $ Left "instance not available"
  inst <- case x of
    Nothing -> randomInstance list
    Just n -> case drop n list of
      x':_ -> return x'
      []     -> do
        when (isNothing $ T.timeout (alloyC config))
          $ lift $ except $ Left "instance not available"
        randomInstance list
  lift $ taskF parseF inst (layoutF config)
  where
    randomInstance list = do
      n <- randomInSegment segment ((length list - segment) `div` 4)
      return $ list !! n

randomInSegment :: (RandomGen g, Monad m) => Int -> Int -> RandT g m Int
randomInSegment segment segLength = do
  x <- liftRandT $ return . randomR ((0,) $ pred segLength)
  return $ segment + 4 * x
