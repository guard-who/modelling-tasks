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
  findInitial,
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

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromList,
  keys,
  foldrWithKey,
  insert,
  )
import qualified Data.Set                         as Set (
  Set,
  toList,
  )

import Modelling.Auxiliary.Common                 (oneOf)
import Modelling.Auxiliary.Output (
  LangM',
  LangM,
  Language (English, German),
  OutputMonad (..),
  Rated,
  english,
  german,
  hoveringInformation,
  localise,
  singleChoice,
  translate,
  translations,
  )
import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  defaultConstraints,
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
import Modelling.PetriNet.Diagram       (drawNet, getDefaultNet, getNet)
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Reach.Type    (Transition (Transition))
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  DrawSettings (..),
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PetriConflict (Conflict, conflictTrans),
  PetriLike,
  PetriNet,
  PickConcurrencyConfig (..), PickConflictConfig (..),
  placeNames,
  randomDrawSettings,
  shuffleNames,
  transitionNames,
  traversePetriLike,
  )

import Modelling.PetriNet.Reach.Group (groupSVG)

import Control.Arrow                    (Arrow (second))
import Control.Monad.Random (
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Bitraversable               (bimapM)
import Data.List                        (nub)
import Data.Map                         (Map)
import Data.Maybe                       (isJust, isNothing)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (renderSVG)
import Diagrams.Prelude                 (mkWidth)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance, Object, getSingle, lookupSig, unscoped
  )
import System.Random.Shuffle            (shuffleM)
import Text.Read                        (readMaybe)

data FindInstance a = FindInstance {
  drawFindWith :: DrawSettings,
  transitionPair :: a,
  net :: PetriLike String,
  numberOfPlaces :: Int
  }
  deriving (Generic, Read, Show)

newtype PickInstance = PickInstance {
  nets :: Map Int (Bool, PetriNet)
  }
  deriving (Generic, Read, Show)

findConcurrencyTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance (Concurrent String)
  -> LangM m
findConcurrencyTask path task = do
  pn <- renderWith path "concurrent" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english "Considering this Petri net"
    german "Betrachten Sie dieses Petrinetz"
  image pn
  paragraph $ translate $ do
    english "Which pair of transitions are concurrently activated under the initial marking?"
    german "Welches Paar von Transitionen ist unter der Startmarkierung nebenläufig aktiviert?"
  paragraph $ do
    translate $ do
      english "Please state your answer by giving a pair of concurrently activated transitions. "
      german "Geben Sie Ihre Antwort durch Eingabe eines Paars von nebenläufig aktivierten Transitionen an. "
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code $ show findInitial
    translate $ do
      let t1 = show $ fst findInitial
          t2 = show $ snd findInitial
      english [i| as answer would indicate that transitions #{t1} and #{t2} are concurrently activated under the initial marking. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung nebenläufig aktiviert sind. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

findInitial :: (Transition, Transition)
findInitial = (Transition 0, Transition 1)

findConcurrencyEvaluation
  :: OutputMonad m
  => FindInstance (Concurrent String)
  -> (String, String)
  -> LangM m
findConcurrencyEvaluation task = do
  let what = translations $ do
        english "are concurrent activated"
        german "sind nebenläufig aktiviert"
  transitionPairEvaluation what (numberOfPlaces task) (ft, st)
  where
    Concurrent (ft, st) = transitionPair task

transitionPairEvaluation
  :: OutputMonad m
  => Map Language String
  -> Int
  -> (String, String)
  -> (String, String)
  -> LangM m
transitionPairEvaluation what n (ft, st) is = do
  paragraph $ translate $ do
    english "Remarks on your solution:"
    german "Anmerkungen zu Ihrer Lösung:"
  assertion (isTransition fi) $ translate $ do
    english $ fi ++ " is a valid transition of the given Petri net?"
    german $ fi ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
  assertion (isTransition si) $ translate $ do
    english $ si ++ " is a valid transition of the given Petri net?"
    german $ si ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
  assertion (ft == fi && st == si || ft == si && st == fi) $ translate $ do
    english $ "Given transitions " ++ localise English what ++ "?"
    german $ "Die angegebenen Transitionen " ++ localise German what ++ "?"
  where
    (fi, si) = is
    isTransition xs
      | 't':xs' <- xs
      , Just x  <- readMaybe xs'
      = x >= 1 && x <= n
      | otherwise
      = False

findConflictTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance Conflict
  -> LangM m
findConflictTask path task = do
  pn <- renderWith path "conflict" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english "Considering this Petri net"
    german "Betrachten Sie folgendes Petrinetz"
  image pn
  paragraph $ translate $ do
    english "Which pair of transitions are in conflict under the initial marking?"
    german "Welches Paar von Transitionen steht unter der Startmarkierung in Konflikt?"
  paragraph $ do
    translate $ do
      english "Please state your answer by giving a pair of conflicting transitions. "
      german "Geben Sie Ihre Antwort durch Eingabe eines Paars von in Konflikt stehenden Transitionen an. "
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code $ show findInitial
    translate $ do
      let t1 = show $ fst findInitial
          t2 = show $ snd findInitial
      english [i| as answer would indicate that transitions #{t1} and #{t2} are in conflict under the initial marking. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung in Konflikt stehen. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

findConflictEvaluation
  :: OutputMonad m
  => FindInstance Conflict
  -> (String, String)
  -> LangM m
findConflictEvaluation task = do
  let what = translations $ do
        english "have a conflict"
        german "haben einen Konflikt"
  transitionPairEvaluation what (numberOfPlaces task) (ft, st)
  where
    (ft, st) = conflictTrans $ transitionPair task

pickConcurrencyTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> PickInstance
  -> LangM m
pickConcurrencyTask path task = do
  paragraph $ translate $ do
    english "Which of the following Petri nets has exactly one pair of transitions that are concurrently activated?"
    german "Welches dieser Petrinetze hat genau ein Paar von Transitionen, die nebenläufig aktiviert sind?"
  files <- renderPick path "concurrent" task
  images show snd files
  paragraph $ translate $ do
    english "Please state your answer by giving only the number of the Petri net having these concurrently activated transitions. "
    german "Geben Sie Ihre Antwort durch Eingabe der Nummer des Petrinetzes an, das diese nebenläufig aktivierten Transitionen hat. "
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that Petri net 1 has exactly two transitions that are concurrently activated (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]
      german $ "als Antwort würde bedeuten, dass Petrinetz 1 genau zwei nebenläufig aktivierte Transitionen hat (und dass "
        ++ (if plural
            then "die anderen Petrinetze dies nicht tun"
            else "das andere Petrinetz dies nicht tut")
        ++ ")."
  paragraph hoveringInformation

wrongInstances :: PickInstance -> Int
wrongInstances inst = length [False | (False, _) <- M.elems (nets inst)]

pickEvaluation
  :: OutputMonad m
  => PickInstance
  -> Int
  -> Rated m
pickEvaluation = do
  let what = translations $ do
        english "petri net"
        german "Petrinetz"
  singleChoice what Nothing . head . M.keys . M.filter fst . nets

pickConflictTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> PickInstance
  -> LangM m
pickConflictTask path task = do
  paragraph $ translate $ do
    english "Which of the following Petri nets has exactly one pair of transitions that are in conflict?"
    german "Welches dieser Petrinetze hat genau ein Paar von Transitionen, die in Konflikt stehen?"
  files <- renderPick path "conflict" task
  images show snd files
  paragraph $ translate $ do
    english "Please state your answer by giving only the number of the Petri net having these transitions in conflict. "
    german "Geben Sie Ihre Antwort durch Eingabe der Nummer des Petrinetzes an, das diese in Konflikt stehenden Transitionen hat. "
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that Petri net 1 has exactly two transitions that are in conflict (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]
      german $ "als Antwort würde bedeuten, dass Petrinetz 1 genau zwei in Konflikt stehende Transitionen hat (und dass "
        ++ (if plural
            then "die anderen Petrinetze dies nicht tun"
            else "das andere Petrinetz dies nicht tut")
        ++ ")."
  paragraph hoveringInformation

findConcurrencyGenerate
  :: FindConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance (Concurrent String))
findConcurrencyGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findConcurrency config segment
  gc <- oneOf $ graphLayout bc
  return $ FindInstance {
    drawFindWith   = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = gc
      },
    transitionPair = c,
    net = d,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConcurrencyConfig)

findConcurrency
  :: RandomGen g
  => FindConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, Concurrent String)
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  (\c -> alloyConfig (c :: FindConcurrencyConfig))

findConflictGenerate
  :: FindConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance Conflict)
findConflictGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findConflict config segment
  gc <- oneOf $ graphLayout bc
  return $ FindInstance {
    drawFindWith = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = gc
      },
    transitionPair = c,
    net = d,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConflictConfig)

findConflict
  :: RandomGen g
  => FindConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, Conflict)
findConflict = taskInstance
  findTaskInstance
  petriNetFindConfl
  parseConflict
  (\c -> alloyConfig (c :: FindConflictConfig))

pickConcurrencyGenerate
  :: PickConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConcurrencyGenerate = pickGenerate pickConcurrency bc
  where
    bc config = basicConfig (config :: PickConcurrencyConfig)

pickConflictGenerate
  :: PickConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConflictGenerate = pickGenerate pickConflict bc
  where
    bc config = basicConfig (config :: PickConflictConfig)

pickGenerate
  :: (c -> Int -> RandT StdGen (ExceptT String IO) [(PetriLike String, Maybe a)])
  -> (c -> BasicConfig)
  -> c
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickGenerate pick bc config segment seed = flip evalRandT (mkStdGen seed) $ do
  ns <- pick config segment
  ns'  <- shuffleM ns
  let ts = nub $ concat $ transitionNames . fst <$> ns'
      ps = nub $ concat $ placeNames . fst <$> ns'
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  ns'' <- lift $ bimapM (traversePetriLike (`BM.lookup` mapping)) return `mapM` ns'
  ns''' <- mapM (\(n, m) -> (n,,m) <$> randomDrawSettings (bc config)) ns''
  return $ PickInstance {
    nets = M.fromList $ zip [1 ..] [(isJust m, (n, d)) | (n, d, m) <- ns''']
    }

renderWith
  :: (MonadIO m, OutputMonad m)
  => String
  -> String
  -> PetriLike String
  -> DrawSettings
  -> LangM' m FilePath
renderWith path task net config = do
  f <- lift $ liftIO $ runExceptT $ do
    let file = path ++ task ++ ".svg"
    dia <- drawNet id net
      (not $ withPlaceNames config)
      (not $ withTransitionNames config)
      (not $ with1Weights config)
      (withGraphvizCommand config)
    liftIO $ renderSVG file (mkWidth 250) dia
    liftIO $ groupSVG file
    return file
  either
    (const $ (>> return "") $ refuse $ translate $ do
      english "Drawing diagram failed!"
      german "Diagrammzeichnen fehlgeschlagen!"
    )
    return
    f

renderPick
  :: (MonadIO m, OutputMonad m)
  => String
  -> String
  -> PickInstance
  -> LangM' m (Map Int (Bool, String))
renderPick path task config =
  M.foldrWithKey render (return mempty) $ nets config
  where
    render x (b, (net, ds)) ns = do
      file <- renderWith path (task ++ '-' : show x) net ds
      M.insert x (b, file) <$> ns

pickConcurrency
  :: RandomGen g
  => PickConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) [(PetriLike String, Maybe (Concurrent String))]
pickConcurrency = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  (\c -> alloyConfig (c :: PickConcurrencyConfig))

pickConflict
  :: RandomGen g
  => PickConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) [(PetriLike String, Maybe Conflict)]
pickConflict = taskInstance
  pickTaskInstance
  petriNetPickConfl
  parseConflict
  (\c -> alloyConfig (c :: PickConflictConfig))

findTaskInstance
  :: (RandomGen g, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> RandT g (ExceptT String IO) (PetriLike String, t String)
findTaskInstance f inst = do
  (pl, t) <- lift $ getNet f inst
  (pl', mapping) <- shuffleNames pl
  t'  <- lift $ (`BM.lookup` mapping) `mapM` t
  return (pl', t')

pickTaskInstance
  :: (MonadTrans m, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> m (ExceptT String IO) [(PetriLike String, Maybe (t String))]
pickTaskInstance parseF inst = lift $ do
  confl <- second Just <$> getNet parseF inst
  net   <- (,Nothing) <$> getDefaultNet inst
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
      (defaultConstraints activatedDefault basicC)
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
