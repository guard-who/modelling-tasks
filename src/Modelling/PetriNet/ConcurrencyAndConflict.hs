{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  FindInstance (..),
  checkFindConcurrencyConfig, checkFindConflictConfig,
  checkPickConcurrencyConfig, checkPickConflictConfig,
  findConcurrency,
  findConcurrencyEvaluation,
  findConcurrencyGenerate,
  findConcurrencyTask,
  findConflict,
  findConflictEvaluation,
  findConflictGenerate,
  findConflictTask,
  findTaskInstance,
  parseConcurrency,
  parseConflict,
  petriNetFindConcur, petriNetFindConfl,
  petriNetPickConcur, petriNetPickConfl,
  pickConcurrency, pickConcurrencyTask,
  pickConflicts, pickConflictsTask,
  pickTaskInstance,
  ) where

import qualified Modelling.PetriNet.Types         as T (
  AlloyConfig (maxInstances, timeout)
  )

import qualified Data.Set                         as Set (
  Set,
  toList,
  )

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  )
import Modelling.PetriNet.Alloy (
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
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram       (getDefaultNet, getNet)
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Types         (
  AdvConfig,
  AlloyConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PetriConflict (Conflict, conflictTrans),
  PickConcurrencyConfig (..), PickConflictConfig (..),
  defaultAlloyConfig,
  )

import Control.Arrow                    (Arrow (second))
import Control.Monad                    (when)
import Control.Monad.Random (
  MonadTrans (lift),
  Random (randomR),
  RandomGen,
  mkStdGen
  )
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Data.Maybe                       (isJust, isNothing)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Prelude                 (Diagram, mkWidth)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig (..), Object,
  defaultCallAlloyConfig, getSingle, lookupSig, unscoped,
  )
import Text.Read                        (readMaybe)

data FindInstance a = FindInstance {
  transitionPair :: a,
  net :: FilePath,
  numberOfPlaces :: Int
  }
  deriving (Generic, Show)

findConcurrencyTask :: OutputMonad m => FindInstance (Concurrent String) -> m ()
findConcurrencyTask task = do
  paragraph "Considering this Petri net"
  image $ net task
  paragraph "Which pair of transitions are in concurrency under the initial marking?"

findConcurrencyEvaluation
  :: OutputMonad m
  => FindInstance (Concurrent String)
  -> (String, String)
  -> m ()
findConcurrencyEvaluation task =
  transitionPairEvaluation "are concurrent" (numberOfPlaces task) (ft, st)
  where
    Concurrent (ft, st) = transitionPair task

transitionPairEvaluation
  :: OutputMonad m
  => String
  -> Int
  -> (String, String)
  -> (String, String)
  -> m ()
transitionPairEvaluation what n (ft, st) is = do
  paragraph "Remarks on your solution:"
  assertion (isTransition fi)
    $ fi ++ " is a valid transition of the given Petri net?"
  assertion (isTransition si)
    $ si ++ " is a valid transition of the given Petri net?"
  assertion (ft == fi && st == si || ft == si && st == fi)
    $ "Given transitions " ++ what ++ "?"
  where
    (fi, si) = is
    isTransition xs
      | 't':xs' <- xs
      , Just x  <- readMaybe xs'
      = x >= 1 && x <= n
      | otherwise
      = False

findConflictTask :: OutputMonad m => FindInstance Conflict -> m()
findConflictTask task = do
  paragraph "Considering this Petri net"
  image $ net task
  paragraph "Which of the following Petrinets doesn't have a conflict?"

findConflictEvaluation
  :: OutputMonad m
  => FindInstance Conflict
  -> (String, String)
  -> m ()
findConflictEvaluation task =
  transitionPairEvaluation "have a conflict" (numberOfPlaces task) (ft, st)
  where
    (ft, st) = conflictTrans $ transitionPair task

pickConcurrencyTask :: String
pickConcurrencyTask = do
  "Which of the following Petri nets does not have a concurrency?"

pickConflictsTask :: String
pickConflictsTask = do
  "Which pair of transitions are in conflict under the initial marking?"

findConcurrencyGenerate
  :: FindConcurrencyConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance (Concurrent String))
findConcurrencyGenerate config path segment seed = do
  (d, c) <- findConcurrency config segment seed
  let file = path ++ "concurrent.svg"
  lift (renderSVG file (mkWidth 250) d)
  return $ FindInstance {
    transitionPair = c,
    net = file,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConcurrencyConfig)

findConcurrency
  :: FindConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO (Diagram B, Concurrent String)
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  (\c -> graphLayout $ basicConfig (c :: FindConcurrencyConfig))
  (\c -> alloyConfig (c :: FindConcurrencyConfig))

findConflictGenerate
  :: FindConflictConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance Conflict)
findConflictGenerate config path segment seed = do
  (d, c) <- findConflict config segment seed
  let file = path ++ "conflict.svg"
  lift (renderSVG file (mkWidth 250) d)
  return $ FindInstance {
    transitionPair = c,
    net = file,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConflictConfig)

findConflict
  :: FindConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO (Diagram B, Conflict)
findConflict = taskInstance
  findTaskInstance
  petriNetFindConfl
  parseConflict
  (\c -> graphLayout $ basicConfig (c :: FindConflictConfig))
  (\c -> alloyConfig (c :: FindConflictConfig))

pickConcurrency
  :: PickConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO [(Diagram B, Maybe (Concurrent String))]
pickConcurrency  = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  (\c -> graphLayout $ basicConfig (c :: PickConcurrencyConfig))
  (const defaultAlloyConfig)

pickConflicts
  :: PickConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflicts = taskInstance
  pickTaskInstance
  petriNetPickConfl
  parseConflict
  (\c -> graphLayout $ basicConfig (c :: PickConflictConfig))
  (const defaultAlloyConfig)

taskInstance
  :: (f -> AlloyInstance -> GraphvizCommand -> ExceptT String IO a)
  -> (config -> String)
  -> f
  -> (config -> GraphvizCommand)
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> Int
  -> ExceptT String IO a
taskInstance taskF alloyF parseF layoutF alloyC config segment seed = do
  let g  = mkStdGen seed
      is = fromIntegral <$> T.maxInstances (alloyC config)
      x  = randomInSegment segment <$> is <*> pure g
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = toInteger . (+1) . fst <$> x,
      timeout      = T.timeout (alloyC config)
      }
    (alloyF config)
  when (null list) $ except $ Left "no instance available"
  when (null $ drop segment list)
    $ except $ Left "instance not available"
  inst <- case x of
    Nothing -> return $ randomInstance list g
    Just (n, g') -> case drop n list of
      x':_ -> return x'
      []     -> do
        when (isNothing $ T.timeout (alloyC config))
          $ except $ Left "instance not available"
        return $ randomInstance list g'
  taskF parseF inst (layoutF config)
  where
    randomInstance list g =
      let (n, _) = randomInSegment segment ((length list - segment) `div` 4) g
      in list !! n

randomInSegment :: RandomGen g => Int -> Int -> g -> (Int, g)
randomInSegment segment segLength g = (segment + 4 * x, g')
  where
    (x, g') = randomR ((0,) $ pred segLength) g

findTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, t String)
findTaskInstance = getNet

pickTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe (t String))]
pickTaskInstance parseF inst gc = do
  confl <- second Just <$> getNet parseF inst gc
  net   <- (,Nothing) <$> getDefaultNet inst gc
  return [confl,net]

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

pred #{predicate}[#{place}#{defaultActivTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{maybe "" multiplePlaces muniquePlace}
  #{constraints}
  #{compConstraints}
}

run #{predicate} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActivTrans"
    compConstraints = maybe
      [i|
  #{connected "defaultGraphIsConnected" $ isConnected basicC}
  #{isolated "defaultNoIsolatedNodes" $ isConnected basicC}
  \##{activatedDefault} >= #{atLeastActive basicC}
  theActivatedDefaultTransitions[#{activatedDefault}]|]
      compAdvConstraints
      specific
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
  no x,y : givenTransitions | x != y and concurrentDefault[x + y]
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
  all u,v : Transitions |
    u != v and concurrent[u + v] implies #{t1} + #{t2} = u + v|]
    defaultActivTrans
      | isNothing specific = [i|#{activatedDefault} : set givenTransitions,|]
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

concurrencyPredicateName :: String
concurrencyPredicateName = "showConcurrency"

conflictPredicateName :: String
conflictPredicateName = "showConflict"

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

{-|
Parses the conflict Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConflict :: AlloyInstance -> Either String (PetriConflict Object)
parseConflict inst = do
  tc1 <- unscopedSingleSig inst conflictTransition1 ""
  tc2 <- unscopedSingleSig inst conflictTransition2 ""
  pc  <- unscopedSingleSig inst conflictPlaces1 ""
  flip Conflict (Set.toList pc)
    <$> ((,) <$> asSingleton tc1 <*> asSingleton tc2)

{-|
Parses the concurrency Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConcurrency :: AlloyInstance -> Either String (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

unscopedSingleSig :: AlloyInstance -> String -> String -> Either String (Set.Set Object)
unscopedSingleSig inst st nd = do
  sig <- lookupSig (unscoped st) inst
  getSingle nd sig

checkFindConcurrencyConfig :: FindConcurrencyConfig -> Maybe String
checkFindConcurrencyConfig FindConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConcurrencyConfig :: PickConcurrencyConfig -> Maybe String
checkPickConcurrencyConfig PickConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig
