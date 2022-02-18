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
  defaultConstraints,
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

import Modelling.Auxiliary.Common       (upperFirst)
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
(Besides 'defaultConstraints')
-}
compBasicConstraints
  :: String
  -- ^ The name of the Alloy variable for the set of activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
compBasicConstraints = enforceConstraints False

{-|
A set of constraints enforcing settings of 'BasicConfig' for the net under
default conditions.
-}
defaultConstraints
  :: String
  -- ^ The name of the Alloy variable for the set of default activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
defaultConstraints = enforceConstraints True

enforceConstraints
  :: Bool
  -- ^ If to generate constraints under default conditions.
  -> String
  -- ^ The name of the Alloy variable for the set of activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
enforceConstraints underDefault activated BasicConfig {
  atLeastActive,
  isConnected,
  maxFlowOverall,
  maxFlowPerEdge,
  maxTokensOverall,
  maxTokensPerPlace,
  minFlowOverall,
  minTokensOverall
  } = [i|
  let t = (sum p : #{places} | p.#{tokens}) |
    t >= #{minTokensOverall} and t <= #{maxTokensOverall}
  all p : #{places} | p.#{tokens} =< #{maxTokensPerPlace}
  all weight : #{nodes}.#{flow}[#{nodes}] | weight =< #{maxFlowPerEdge}
  let theflow = (sum f, t : #{nodes} | f.#{flow}[t]) |
    theflow >= #{minFlowOverall} and #{maxFlowOverall} >= theflow
  \##{activated} >= #{atLeastActive}
  theActivated#{upperFirst which}Transitions[#{activated}]
  #{connected (prepend "graphIsConnected") isConnected}
  #{isolated (prepend "noIsolatedNodes") isConnected}|]
  where
    (given, prepend, which)
      | underDefault = (("given" ++), (which ++) . upperFirst, "default")
      | otherwise    = (id, id, "")
    flow = prepend "flow"
    nodes = given "Nodes"
    places = given "Places"
    tokens = prepend "tokens"

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
  => (f
    -> AlloyInstance
    -> RandT g (ExceptT String IO) a)
  -> (config -> String)
  -> f
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> RandT g (ExceptT String IO) a
taskInstance taskF alloyF parseF alloyC config segment = do
  let is = T.maxInstances (alloyC config)
  list <- lift $ getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = is,
      timeout      = T.timeout (alloyC config)
      }
    (alloyF config)
  when (null $ drop segment list)
    $ lift $ except $ Left "instance not available"
  inst <- case fromIntegral <$> is of
    Nothing -> randomInstance list
    Just n -> do
      x <- randomInSegment segment n
      case drop x list of
        x':_ -> return x'
        []   -> randomInstance list
  taskF parseF inst
  where
    randomInstance list = do
      n <- randomInSegment segment (1 + ((length list - segment - 1) `div` 4))
      return $ list !! n

randomInSegment :: (RandomGen g, Monad m) => Int -> Int -> RandT g m Int
randomInSegment segment segLength = do
  x <- liftRandT $ return . randomR ((0,) $ pred segLength)
  return $ segment + 4 * x
