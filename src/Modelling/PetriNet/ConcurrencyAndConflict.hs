{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  ConflictPlaces,
  FindInstance (..),
  PickInstance (..),
  checkFindConcurrencyConfig, checkFindConflictConfig,
  checkPickConcurrencyConfig, checkPickConflictConfig,
  conflictPlacesShow,
  findConcurrency,
  findConcurrencyEvaluation,
  findConcurrencyGenerate,
  findConcurrencySyntax,
  findConcurrencyTask,
  findConflict,
  findConflictEvaluation,
  findConflictGenerate,
  findConflictPlacesEvaluation,
  findConflictSyntax,
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
  pickSyntax,
  pickTaskInstance,
  renderWith,
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

import Modelling.Auxiliary.Common                 (oneOf, upperFirst)
import Modelling.Auxiliary.Output (
  LangM',
  LangM,
  Language (English, German),
  OutputMonad (..),
  Rated,
  addPretext,
  continueOrAbort,
  english,
  german,
  hoveringInformation,
  localise,
  printSolutionAndAssert,
  recoverFrom,
  singleChoice,
  singleChoiceSyntax,
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
  signatures,
  taskInstance,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick, checkConflictConfig
  )
import Modelling.PetriNet.Diagram       (drawNet, getDefaultNet, getNet)
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Reach.Type (
  Place,
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  Transition (Transition),
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Reach.Group   (writeSVG)
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  ConflictConfig (..),
  DrawSettings (..),
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PetriConflict (Conflict, conflictTrans),
  PetriConflict' (PetriConflict', toPetriConflict),
  PetriLike,
  PetriNet,
  PickConcurrencyConfig (..), PickConflictConfig (..),
  conflictPlaces,
  lConflictPlaces,
  manyRandomDrawSettings,
  placeNames,
  randomDrawSettings,
  shuffleNames,
  transitionNames,
  traversePetriLike,
  )

import Control.Applicative              (Alternative, (<|>))
import Control.Arrow                    (Arrow (second), ArrowChoice (left))
import Control.Lens                     ((.~), makeLensesFor, over)
import Control.Monad                    (forM_, unless)
import Control.Monad.Random (
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except, runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bitraversable               (Bitraversable (bitraverse), bimapM)
import Data.Bool                        (bool)
import Data.Containers.ListUtils        (nubOrd)
import Data.Function                    ((&))
import Data.List                        (partition)
import Data.List.Extra                  (nubSort)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, isJust, isNothing)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance, Object, getSingle, lookupSig, unscoped
  )
import System.Random.Shuffle            (shuffleM)
import Text.Parsec                      (parse)
import Text.Parsec.String               (Parser)

data FindInstance a = FindInstance {
  drawFindWith :: DrawSettings,
  toFind :: a,
  net :: PetriLike String,
  numberOfPlaces :: Int,
  numberOfTransitions :: Int,
  showSolution :: Bool
  }
  deriving (Functor, Generic, Read, Show)

makeLensesFor [("toFind", "lToFind")] ''FindInstance

data PickInstance = PickInstance {
  nets :: Map Int (Bool, PetriNet),
  showSolution :: Bool
  }
  deriving (Generic, Read, Show)

findConcurrencyTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance (Concurrent Transition)
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
    code $ show findInitialShow
    translate $ do
      let t1 = show $ fst findInitialShow
          t2 = show $ snd findInitialShow
      english [i| as answer would indicate that transitions #{t1} and #{t2} are concurrently activated under the initial marking. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung nebenläufig aktiviert sind. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

findInitialShow :: (ShowTransition, ShowTransition)
findInitialShow = bimap ShowTransition ShowTransition findInitial

findInitial :: (Transition, Transition)
findInitial = (Transition 0, Transition 1)

findConcurrencySyntax
  :: OutputMonad m
  => FindInstance (Concurrent Transition)
  -> (Transition, Transition)
  -> LangM' m ()
findConcurrencySyntax task = toFindSyntax withSol $ numberOfTransitions task
  where
    withSol = showSolution (task :: FindInstance (Concurrent Transition))

findConcurrencyEvaluation
  :: OutputMonad m
  => FindInstance (Concurrent Transition)
  -> (Transition, Transition)
  -> Rated m
findConcurrencyEvaluation task x = do
  let what = translations $ do
        english "are concurrent activated"
        german "sind nebenläufig aktiviert"
  result <- toFindEvaluation what withSol concur x
  uncurry printSolutionAndAssert result
  where
    Concurrent concur = toFind task
    withSol = showSolution (task :: FindInstance (Concurrent Transition))

toFindSyntax
  :: OutputMonad m
  => Bool
  -> Int
  -> (Transition, Transition)
  -> LangM' m ()
toFindSyntax withSol n (fi, si) = addPretext $ do
  assertTransition fi
  assertTransition si
  where
    assert = continueOrAbort withSol
    assertTransition t = assert (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a valid transition of the given Petri net?"
      german $ t' ++ " ist eine gültige Transition des gegebenen Petrinetzes?"
    isValidTransition (Transition x) = x >= 1 && x <= n

toFindEvaluation
  :: (Num a, OutputMonad m)
  => Map Language String
  -> Bool
  -> (Transition, Transition)
  -> (Transition, Transition)
  -> LangM' m (Maybe String, a)
toFindEvaluation what withSol (ft, st) (fi, si) = do
  let correct = ft == fi && st == si || ft == si && st == fi
      points = if correct then 1 else 0
      msolutionString =
        if withSol
        then Just $ show $ bimap ShowTransition ShowTransition (ft, st)
        else Nothing
  assert correct $ translate $ do
    english $ "Given transitions " ++ localise English what ++ "?"
    german $ "Die angegebenen Transitionen " ++ localise German what ++ "?"
  return (msolutionString, points)
  where
    assert = continueOrAbort withSol

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
    code $ show findInitialShow
    translate $ do
      let t1 = show $ fst findInitialShow
          t2 = show $ snd findInitialShow
      english [i| as answer would indicate that transitions #{t1} and #{t2} are in conflict under the initial marking. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung in Konflikt stehen. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

findConflictSyntax
  :: OutputMonad m
  => FindInstance Conflict
  -> (Transition, Transition)
  -> LangM' m ()
findConflictSyntax task = toFindSyntax withSol $ numberOfTransitions task
  where
    withSol = showSolution (task :: FindInstance Conflict)

findConflictEvaluation
  :: (Alternative m, OutputMonad m)
  => FindInstance Conflict
  -> (Transition, Transition)
  -> Rated m
findConflictEvaluation task x = findConflictPlacesEvaluation
  (task & lToFind . lConflictPlaces .~ [])
  (x, [])

type ConflictPlaces = ((Transition, Transition), [Place])

conflictPlacesShow
  :: ConflictPlaces
  -> ((ShowTransition, ShowTransition), [ShowPlace])
conflictPlacesShow = bimap
  (bimap ShowTransition ShowTransition)
  (fmap ShowPlace)

findConflictPlacesEvaluation
  :: (Alternative m, OutputMonad m)
  => FindInstance Conflict
  -> ConflictPlaces
  -> Rated m
findConflictPlacesEvaluation task (conflict, ps) = do
  let what = translations $ do
        english "have a conflict"
        german "haben einen Konflikt"
  (ms, res) <- toFindEvaluation what withSol conf conflict
  recoverFrom $ unless (null sources || res == 0) $ do
    forM_ ps' $ \x -> assert (x `elem` sources) $ translate $ do
      let x' = show $ ShowPlace x
      english $ x' ++ " is reason for the conflict?"
      german $ x' ++ " ist auslösende Stelle für den Konflikt?"
    assert (ps' == sources) $ translate $ do
      english "The given solution is correct and complete?"
      german "Die angegebene Lösung ist korrekt und vollständig?"
  let result = min res $ (base - len sources + len correct - len wrong') % base
  printSolutionAndAssert (fixSolution <$> ms) result
  where
    assert = continueOrAbort withSol
    conf = conflictTrans $ toFind task
    sources = conflictPlaces (toFind task)
    fixSolution
      | null sources = id
      | otherwise    = const $ show $ conflictPlacesShow (conf, sources)
    withSol = showSolution (task :: FindInstance Conflict)
    ps' = nubSort ps
    (correct, wrong') = partition (`elem` sources) ps
    base = fromIntegral $ 2 + numberOfPlaces task
    len = fromIntegral . length

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

-- TODO: replace 'wrong' in 'pickGenerate' by 'wrongInstances'
-- if this value might be greater than 1 on task generation.
wrongInstances :: PickInstance -> Int
wrongInstances inst = length [False | (False, _) <- M.elems (nets inst)]

wrong :: Int
wrong = 1

pickSyntax
  :: OutputMonad m
  => PickInstance
  -> Int
  -> LangM m
pickSyntax task = singleChoiceSyntax withSol options
  where
    options = M.keys . M.filter fst $ nets task
    withSol = showSolution (task :: PickInstance)

pickEvaluation
  :: OutputMonad m
  => PickInstance
  -> Int
  -> Rated m
pickEvaluation task = do
  let what = translations $ do
        english "petri net"
        german "Petrinetz"
  singleChoice what msolutionString solution
  where
    msolutionString =
      if withSol
      then Just $ show solution
      else Nothing
    solution = head . M.keys . M.filter fst $ nets task
    withSol = showSolution (task :: PickInstance)

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
  -> ExceptT String IO (FindInstance (Concurrent Transition))
findConcurrencyGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findConcurrency config segment
  gc <- oneOf $ graphLayout bc
  c' <- lift $ except $ traverse
     (parseWith parseTransitionPrec)
     c
  return $ FindInstance {
    drawFindWith   = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = gc
      },
    toFind = c',
    net = d,
    numberOfPlaces = places bc,
    numberOfTransitions = transitions bc,
    showSolution = printSolution (config :: FindConcurrencyConfig)
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
  c' <- lift $ except $ bitraverse
    (parseWith parsePlacePrec)
    (parseWith parseTransitionPrec)
    $ toPetriConflict c
  return $ FindInstance {
    drawFindWith = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = gc
      },
    toFind = over lConflictPlaces nubSort c',
    net = d,
    numberOfPlaces = places bc,
    numberOfTransitions = transitions bc,
    showSolution = printSolution (config :: FindConflictConfig)
    }
  where
    bc = basicConfig (config :: FindConflictConfig)

parseWith :: (Int -> Parser a) -> String -> Either String a
parseWith f = left show . parse (f 0) ""

findConflict
  :: RandomGen g
  => FindConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, PetriConflict' String)
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
pickConcurrencyGenerate = pickGenerate pickConcurrency bc ud ws
  where
    bc config = basicConfig (config :: PickConcurrencyConfig)
    ud config = useDifferentGraphLayouts (config :: PickConcurrencyConfig)
    ws config = printSolution (config :: PickConcurrencyConfig)

pickConflictGenerate
  :: PickConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConflictGenerate = pickGenerate pickConflict bc ud ws
  where
    bc config = basicConfig (config :: PickConflictConfig)
    ud config = useDifferentGraphLayouts (config :: PickConflictConfig)
    ws config = printSolution (config :: PickConflictConfig)

pickGenerate
  :: (c -> Int -> RandT StdGen (ExceptT String IO) [(PetriLike String, Maybe a)])
  -> (c -> BasicConfig)
  -> (c -> Bool)
  -> (c -> Bool)
  -> c
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickGenerate pick bc useDifferent withSol config segment seed = flip evalRandT (mkStdGen seed) $ do
  ns <- pick config segment
  ns'  <- shuffleM ns
  let ts = nubOrd $ concat $ transitionNames . fst <$> ns'
      ps = nubOrd $ concat $ placeNames . fst <$> ns'
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  ns'' <- lift $ bimapM (traversePetriLike (`BM.lookup` mapping)) return `mapM` ns'
  s <- randomDrawSettings (bc config)
  ns''' <- addDrawingSettings s ns''
  return $ PickInstance {
    nets = M.fromList $ zip [1 ..] [(isJust m, (n, d)) | ((n, m), d) <- ns'''],
    showSolution = withSol config
    }
  where
    addDrawingSettings s ps = zip ps <$>
      if useDifferent config
      then manyRandomDrawSettings (bc config) (wrong + 1)
      else return $ replicate (wrong + 1) s

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
    liftIO $ writeSVG file dia
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
  -> RandT g (ExceptT String IO) [(PetriLike String, Maybe (PetriConflict' String))]
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
  conflictConfig,
  uniqueConflictPlace
  }
  = petriNetAlloy
    basicConfig
    changeConfig
    (Just (conflictConfig, uniqueConflictPlace))
    $ Just advConfig

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl PickConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig,
  uniqueConflictPlace
  }
  = petriNetAlloy
    basicConfig
    changeConfig
    (Just (conflictConfig, uniqueConflictPlace))
    Nothing

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
  -> Maybe (ConflictConfig, Maybe Bool)
  -- ^ Just for conflict task; Nothing for concurrency task
  -> Maybe AdvConfig
  -- ^ Just for find task; Nothing for pick task
  -> String
petriNetAlloy basicC changeC muniquePlace specific
  = [i|module #{moduleName}

#{modulePetriSignature}
#{maybe sigs (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{predicate}[#{place}#{defaultActivTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{maybe "" (multiplePlaces . snd) muniquePlace}
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
    conflictConfig = fst $ fromJust muniquePlace
    constraints :: String
    constraints
      | conflict  = [i|
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  all q : #{p} | conflict[#{t1}, #{t2}, q]
  no q : (Places - #{p}) | conflict[#{t1}, #{t2}, q]
  all u,v : Transitions, q : Places |
    conflict[u,v,q] implies #{t1} + #{t2} = u + v
  #{preconditions ""}
  #{preconditions "Default"}
  #{conflictDistractor "" ""}
  #{conflictDistractor "given" "default"}|]
      | otherwise = [i|
  no x,y : givenTransitions | x != y and concurrentDefault[x + y]
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
  all u,v : Transitions |
    u != v and concurrent[u + v] implies #{t1} + #{t2} = u + v|]
    preconditions :: String -> String
    preconditions which = flip foldMap (addConflictCommonPreconditions conflictConfig)
      $ \case
      True  -> [i|some (common#{which}Preconditions[#{t1}, #{t2}] - #{p})|]
      False -> [i|no (common#{which}Preconditions[#{t1}, #{t2}] - #{p})|]
    conflictDistractor :: String -> String -> String
    conflictDistractor what which = flip foldMap (withConflictDistractors conflictConfig) $ \x ->
      [i|let ts = #{what}Transitions - #{t1} - #{t2} |
    |] ++
        let op = conflictDistractorAddExtraPreconditions conflictConfig
                   & maybe ">=" (bool "=" ">")
            prepend = if null which then id else (which ++) . upperFirst
            tokens  = prepend "tokens"
            flow    = prepend "flow"
            distractorConflictLike = conflictDistractorOnlyConflictLike conflictConfig
              & bool "" [i|all p : ps | p.#{tokens} >= p.#{flow}[t1] and p.#{tokens} >= p.#{flow}[t2]
        some p : ps | p.#{tokens} < plus[p.#{flow}[t1], p.#{flow}[t2]]|]
            distractorConcurrentLike = conflictDistractorOnlyConcurrentLike conflictConfig
              & bool "" [i|all p : ps | p.#{tokens} >= plus[p.#{flow}[t1], p.#{flow}[t2]]|]
        in if x
        then [i|some t1 : ts | one t2 : ts - t1 |
      let ps = common#{upperFirst which}Preconditions[t1, t2] {
        \#ps #{op} \##{p}
        #{distractorConflictLike}
        #{distractorConcurrentLike}
      }|]
        else [i|no t1, t2 : ts |
      let ps = common#{upperFirst which}Preconditions[t1, t2] |
        \#ps > 1 and all p : ps | p.#{tokens} >= p.#{flow}[t1] and p.#{tokens} >= p.#{flow}[t2]|]
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
    sigs = signatures "given" (places basicC) (transitions basicC)
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
parseConflict :: AlloyInstance -> Either String (PetriConflict' Object)
parseConflict inst = do
  tc1 <- unscopedSingleSig inst conflictTransition1 ""
  tc2 <- unscopedSingleSig inst conflictTransition2 ""
  pc  <- unscopedSingleSig inst conflictPlaces1 ""
  PetriConflict' . flip Conflict (Set.toList pc)
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
  changeConfig,
  useDifferentGraphLayouts
  }
  = checkConfigForPick useDifferentGraphLayouts wrong basicConfig changeConfig

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig
  }
  = checkConfigForFind basicConfig changeConfig
  <|> checkConflictConfig basicConfig conflictConfig

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig,
  useDifferentGraphLayouts
  }
  = checkConfigForPick useDifferentGraphLayouts wrong basicConfig changeConfig
  <|> checkConflictConfig basicConfig conflictConfig
