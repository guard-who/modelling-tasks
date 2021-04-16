{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  FindInstance (..),
  PickInstance (..),
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
  pickConcurrency,
  pickConcurrencyGenerate,
  pickConcurrencyTask,
  pickConflict,
  pickConflictGenerate,
  pickConflictTask,
  pickEvaluation,
  pickTaskInstance,
  ) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as Set (
  Set,
  toList,
  )

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  multipleChoice,
  LangM,
  )
import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  connected,
  isolated,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitwidth,
  petriScopeMaxSeq,
  taskInstance,
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
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PetriConflict (Conflict, conflictTrans),
  PickConcurrencyConfig (..), PickConflictConfig (..),
  )

import Control.Arrow                    (Arrow (second))
import Control.Monad.Random (
  MonadTrans (lift),
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.Trans.Except       (ExceptT)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Data.Map                         (Map)
import Data.Maybe                       (isJust, isNothing)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Prelude                 (Diagram, mkWidth)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance, Object, getSingle, lookupSig, unscoped,
  )
import System.Random.Shuffle            (shuffleM)
import Text.Read                        (readMaybe)

data FindInstance a = FindInstance {
  transitionPair :: a,
  net :: FilePath,
  numberOfPlaces :: Int
  }
  deriving (Generic, Show)

newtype PickInstance = PickInstance {
  nets :: Map Int (Bool, FilePath)
  }
  deriving (Generic, Show)

findConcurrencyTask :: OutputMonad m => FindInstance (Concurrent String) -> LangM m
findConcurrencyTask task = do
  paragraph $ text "Considering this Petri net"
  image $ net task
  paragraph $ text "Which pair of transitions are concurrently activated under the initial marking?"
  paragraph $ do
    text "Please state your answer by giving a pair of concurrently activated transitions. "
    text "Stating as answer: "
    code [i|("t1", "t2")|]
    text " would indicate that transitions t1 and t2 are concurrently activated under the initial marking."
    text " The order of transitions within the pair does not matter here."

findConcurrencyEvaluation
  :: OutputMonad m
  => FindInstance (Concurrent String)
  -> (String, String)
  -> LangM m
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
  -> LangM m
transitionPairEvaluation what n (ft, st) is = do
  paragraph $ text "Remarks on your solution:"
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

findConflictTask :: OutputMonad m => FindInstance Conflict -> LangM m
findConflictTask task = do
  paragraph $ text "Considering this Petri net"
  image $ net task
  paragraph $ text
    "Which pair of transitions are in conflict under the initial marking?"
  paragraph $ do
    text "Please state your answer by giving a pair of conflicting transitions. "
    text "Stating as answer: "
    code [i|("t1", "t2")|]
    text " would indicate that transitions t1 and t2 are in conflict under the initial marking."
    text " The order of transitions within the pair does not matter here. "

findConflictEvaluation
  :: OutputMonad m
  => FindInstance Conflict
  -> (String, String)
  -> LangM m
findConflictEvaluation task =
  transitionPairEvaluation "have a conflict" (numberOfPlaces task) (ft, st)
  where
    (ft, st) = conflictTrans $ transitionPair task

pickConcurrencyTask :: OutputMonad m => PickInstance -> LangM m
pickConcurrencyTask task = do
  paragraph $ text
    "Which of the following Petri nets has exactly two transitions that are concurrently activated?"
  images show snd $ nets task
  paragraph $ text
    [i|Please state your answer by giving only the number of the Petri net having these concurrently activated transitions.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that Petri net 1 has exactly two transitions that are concurrently activated (and the other Petri nets don't!).|]

pickEvaluation
  :: OutputMonad m
  => PickInstance
  -> [Int]
  -> LangM m
pickEvaluation = multipleChoice "petri nets" . nets

pickConflictTask :: OutputMonad m => PickInstance -> LangM m
pickConflictTask task = do
  paragraph $ text
    "Which of the following Petri nets has exactly two transitions that are in conflict?"
  images show snd $ nets task
  paragraph $ text
    [i|Please state your answer by giving only the number of the Petri net having these transitions in conflict.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that Petri net 1 has exactly two transitions that are in conflict (and the other Petri nets don't!).|]

findConcurrencyGenerate
  :: FindConcurrencyConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance (Concurrent String))
findConcurrencyGenerate config path segment seed = do
  (d, c) <- evalRandT (findConcurrency config segment) $ mkStdGen seed
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
  :: RandomGen g
  => FindConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) (Diagram B, Concurrent String)
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  (\c -> basicConfig (c :: FindConcurrencyConfig))
  (\c -> alloyConfig (c :: FindConcurrencyConfig))

findConflictGenerate
  :: FindConflictConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance Conflict)
findConflictGenerate config path segment seed = do
  (d, c) <- evalRandT (findConflict config segment) $ mkStdGen seed
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
  :: RandomGen g
  => FindConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) (Diagram B, Conflict)
findConflict = taskInstance
  findTaskInstance
  petriNetFindConfl
  parseConflict
  (\c -> basicConfig (c :: FindConflictConfig))
  (\c -> alloyConfig (c :: FindConflictConfig))

pickConcurrencyGenerate
  :: PickConcurrencyConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConcurrencyGenerate = pickGenerate pickConcurrency "concurrent"

pickConflictGenerate
  :: PickConflictConfig
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConflictGenerate = pickGenerate pickConflict "conflict"

pickGenerate
  :: (c -> Int -> RandT StdGen (ExceptT String IO) [(Diagram B, Maybe a)])
  -> String
  -> c
  -> FilePath
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickGenerate pick task config path segment seed = do
  ns <- evalRandT (pick config segment) $ mkStdGen seed
  let g  = mkStdGen seed
  ns'  <- evalRandT (shuffleM ns) g
  ns'' <- lift $ foldl render (return []) $ zip [1 ..] ns'
  return $ PickInstance $ M.fromList ns''
  where
    render ns (x, (net, m))= do
      let file = path ++ task ++ "-" ++ show x ++ ".svg"
      renderSVG file (mkWidth 250) net
      ((x, (isJust m, file)) :) <$> ns

pickConcurrency
  :: RandomGen g
  => PickConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) [(Diagram B, Maybe (Concurrent String))]
pickConcurrency = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  (\c -> basicConfig (c :: PickConcurrencyConfig))
  (\c -> alloyConfig (c :: PickConcurrencyConfig))

pickConflict
  :: RandomGen g
  => PickConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) [(Diagram B, Maybe Conflict)]
pickConflict = taskInstance
  pickTaskInstance
  petriNetPickConfl
  parseConflict
  (\c -> basicConfig (c :: PickConflictConfig))
  (\c -> alloyConfig (c :: PickConflictConfig))

findTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, t String)
findTaskInstance = getNet

pickTaskInstance
  :: Traversable t
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe (t String))]
pickTaskInstance parseF inst hidePNames hideTNames hide1 gc = do
  confl <- second Just <$> getNet parseF inst hidePNames hideTNames hide1 gc
  net   <- (,Nothing) <$> getDefaultNet inst hidePNames hideTNames hide1 gc
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
