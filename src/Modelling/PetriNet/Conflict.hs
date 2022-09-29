{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.Conflict (
  ConflictPlaces,
  checkConflictConfig,
  checkFindConflictConfig,
  checkPickConflictConfig,
  conflictPlacesShow,
  defaultFindConflictInstance,
  defaultPickConflictInstance,
  findConflict,
  findConflictEvaluation,
  findConflictGenerate,
  findConflictPlacesSolution,
  findConflictSyntax,
  findConflictTask,
  parseConflict,
  petriNetFindConfl,
  petriNetPickConfl,
  pickConflict,
  pickConflictGenerate,
  pickConflictTask,
  ) where

import qualified Data.Map                         as M (
  empty,
  fromList,
  )
import qualified Data.Set                         as Set (
  toList,
  )

import Modelling.Auxiliary.Common (
  Object,
  oneOf,
  parseWith,
  upperFirst,
  )
import Modelling.Auxiliary.Output (
  hoveringInformation,
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
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  )
import Modelling.PetriNet.Diagram (
  renderWith,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  checkConfigForFind,
  findInitial,
  findTaskInstance,
  lToFind,
  toFindEvaluation,
  toFindSyntax,
  )
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Pick (
  PickInstance (..),
  checkConfigForPick,
  pickGenerate,
  pickTaskInstance,
  renderPick,
  wrong,
  wrongInstances,
  )
import Modelling.PetriNet.Reach.Type (
  Place (Place),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  Transition (Transition),
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Conflict,
  ConflictConfig (..),
  DrawSettings (..),
  FindConflictConfig (..),
  Node (..),
  PetriConflict (Conflict, conflictTrans),
  PetriConflict' (PetriConflict', toPetriConflict),
  PetriLike (PetriLike, allNodes),
  PickConflictConfig (..),
  conflictPlaces,
  lConflictPlaces,
  transitionPairShow,
  )

import Control.Applicative              (Alternative, (<|>))
import Control.Lens                     ((.~), over)
import Control.Monad                    (forM_, unless)
import Control.Monad.Output (
  LangM',
  LangM,
  OutputMonad (..),
  Rated,
  continueOrAbort,
  english,
  german,
  printSolutionAndAssert,
  recoverFrom,
  translate,
  translations,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bitraversable               (Bitraversable (bitraverse))
import Data.Bool                        (bool)
import Data.Either                      (isLeft)
import Data.Function                    ((&))
import Data.GraphViz.Commands           (GraphvizCommand (Circo, Fdp))
import Data.List                        (partition)
import Data.List.Extra                  (nubSort)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
import Language.Alloy.Call (
  AlloyInstance
  )

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
      german "Geben Sie Ihre Antwort durch Angabe eines Paars von in Konflikt stehenden Transitionen an. "
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = transitionPairShow findInitial
    code $ show ts
    translate $ do
      let (t1, t2) = bimap show show ts
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

findConflictSolution :: FindInstance (PetriConflict p t) -> (t, t)
findConflictSolution = conflictTrans . toFind

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
  recoverFrom $ unless (null inducing || res == 0) $ do
    forM_ ps' $ \x -> assert (x `elem` inducing) $ translate $ do
      let x' = show $ ShowPlace x
      english $ x' ++ " is reason for the conflict?"
      german $ x' ++ " ist auslösende Stelle für den Konflikt?"
    assert (ps' == inducing) $ translate $ do
      english "The given solution is correct and complete?"
      german "Die angegebene Lösung ist korrekt und vollständig?"
  let result = min res $ (base - len inducing + len correct - len wrong') % base
  printSolutionAndAssert (fixSolution <$> ms) result
  where
    assert = continueOrAbort withSol
    conf = findConflictSolution task
    inducing = conflictPlaces (toFind task)
    fixSolution
      | null inducing = id
      | otherwise    = const $ show $ conflictPlacesShow (conf, inducing)
    withSol = showSolution (task :: FindInstance Conflict)
    ps' = nubSort ps
    (correct, wrong') = partition (`elem` inducing) ps
    base = fromIntegral $ 2 + numberOfPlaces task
    len = fromIntegral . length

findConflictPlacesSolution :: FindInstance (PetriConflict p t) -> ((t, t), [p])
findConflictPlacesSolution task =
  (findConflictSolution task, conflictPlaces $ toFind task)

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
    german "Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzes an, das diese in Konflikt stehenden Transitionen hat. "
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that Petri net 1 has exactly two transitions that are in conflict (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]
      german $ "als Antwort würde bedeuten, dass Petrinetz 1 genau zwei in Konflikt stehende Transitionen hat (und dass "
        ++ (if plural
            then "die anderen Petrinetze dies nicht tun"
            else "das andere Petrinetz dies nicht tut")
        ++ ")."
  paragraph hoveringInformation

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

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig {
  basicConfig,
  advConfig,
  changeConfig,
  conflictConfig,
  uniqueConflictPlace
  }
  = petriNetConflictAlloy
    basicConfig
    changeConfig
    conflictConfig
    uniqueConflictPlace
    $ Right advConfig

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

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl PickConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig,
  prohibitSourceTransitions,
  uniqueConflictPlace
  }
  = petriNetConflictAlloy
    basicConfig
    changeConfig
    conflictConfig
    uniqueConflictPlace
    (Left prohibitSourceTransitions)

{-|
Generate code for PetriNet conflict tasks
-}
petriNetConflictAlloy
  :: BasicConfig
  -> ChangeConfig
  -> ConflictConfig
  -> Maybe Bool
  -> Either Bool AdvConfig
  -- ^ Right for find task; Left for pick task
  -> String
petriNetConflictAlloy basicC changeC conflictC uniqueConflictP specific
  = [i|module PetriNetConfl

#{modulePetriSignature}
#{either (const sigs) (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{conflictPredicateName}[#{p} : some Places,#{defaultActivTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{multiplePlaces uniqueConflictP}
  #{sourceTransitionConstraints}
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  all q : #{p} | conflict[#{t1}, #{t2}, q]
  no q : (Places - #{p}) | conflict[#{t1}, #{t2}, q]
  all u,v : Transitions, q : Places |
    conflict[u,v,q] implies #{t1} + #{t2} = u + v
  #{preconditions ""}
  #{preconditions "Default"}
  #{conflictDistractor "" ""}
  #{conflictDistractor "given" "default"}
  #{compConstraints}
}

run #{conflictPredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActivTrans"
    compConstraints = either
      (const $ defaultConstraints activatedDefault basicC)
      compAdvConstraints
      specific
    sourceTransitionConstraints
      | Left True <- specific = [i|
  no t : givenTransitions | no givenPlaces.flow[t]
  no t : Transitions | sourceTransitions[t]|]
      | otherwise = ""
    preconditions :: String -> String
    preconditions which = flip foldMap (addConflictCommonPreconditions conflictC)
      $ \case
      True  -> [i|some (common#{which}Preconditions[#{t1}, #{t2}] - #{p})|]
      False -> [i|no (common#{which}Preconditions[#{t1}, #{t2}] - #{p})|]
    conflictDistractor :: String -> String -> String
    conflictDistractor what which = flip foldMap (withConflictDistractors conflictC) $ \x ->
      [i|let ts = #{what}Transitions - #{t1} - #{t2} |
    |] ++
        let op = conflictDistractorAddExtraPreconditions conflictC
                   & maybe ">=" (bool "=" ">")
            prepend = if null which then id else (which ++) . upperFirst
            tokens  = prepend "tokens"
            flow    = prepend "flow"
            distractorConflictLike = conflictDistractorOnlyConflictLike conflictC
              & bool "" [i|all p : ps | p.#{tokens} >= p.#{flow}[t1] and p.#{tokens} >= p.#{flow}[t2]
        some p : ps | p.#{tokens} < plus[p.#{flow}[t1], p.#{flow}[t2]]|]
            distractorConcurrentLike = conflictDistractorOnlyConcurrentLike conflictC
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
      | isLeft specific    = [i|#{activatedDefault} : set givenTransitions,|]
      | otherwise          = ""
    multiplePlaces unique
      | unique == Just True
      = [i|one #{p}|]
      | unique == Just False
      = [i|not (one #{p})|]
      | otherwise
      = ""
    p  = places1
    sigs = signatures "given" (places basicC) (transitions basicC)
    t1 = transition1
    t2 = transition2

conflictPredicateName :: String
conflictPredicateName = "showConflict"

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

checkConflictConfig :: BasicConfig -> ConflictConfig -> Maybe String
checkConflictConfig bc ConflictConfig {
  addConflictCommonPreconditions,
  withConflictDistractors,
  conflictDistractorAddExtraPreconditions,
  conflictDistractorOnlyConflictLike,
  conflictDistractorOnlyConcurrentLike
  }
  | Just True <- withConflictDistractors
  , conflictDistractorOnlyConflictLike == conflictDistractorOnlyConcurrentLike
  = Just "Either 'conflictDistractorOnlyConflictLike' or 'conflictDistractorOnlyConcurrentLike' hast to be set!"
  | Just True <- withConflictDistractors
  , places bc < minPlaces
  = Just $ "Your current conflict configuration requires at least "
    ++ show minPlaces ++ " places."
  | Just True <- withConflictDistractors
  , transitions bc < minTransitions
  = Just $ "Your current conflict configuration requires at least "
    ++ show minTransitions ++ " transitions."
  | Just True <- withConflictDistractors
  = Nothing
  | Just {} <- conflictDistractorAddExtraPreconditions
  = Just "The parameter 'conflictDistractorAddExtraPreconditions' can only be set, if 'withConflictDistractors' is enforced."
  | conflictDistractorOnlyConflictLike
  = Just "The parameter 'conflictDistractorOnlyConflictLike' can only be set, if 'withConflictDistractors' is enforced."
  | conflictDistractorOnlyConcurrentLike
  = Just "The parameter 'conflictDistractorOnlyConcurrentLike' can only be set, if 'withConflictDistractors' is enforced."
  | otherwise
  = Nothing
  where
    minPlaces = (2 +) . sum $
      [1 |  Just True == addConflictCommonPreconditions]
      ++ [1 | Just True ==  withConflictDistractors]
      ++ [1
         | Just True == withConflictDistractors
         , Just True == conflictDistractorAddExtraPreconditions]
    minTransitions = 2 + sum
      [2 | Just True == withConflictDistractors]

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

defaultPickConflictInstance :: PickInstance
defaultPickConflictInstance = PickInstance {
  nets = M.fromList [
    (1,(False,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",PlaceNode {initial = 0, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.fromList [("t2",1)]}),
          ("s3",PlaceNode {initial = 1, flowIn = M.fromList [("t1",1)], flowOut = M.empty}),
          ("s4",PlaceNode {initial = 0, flowIn = M.fromList [("t1",1)], flowOut = M.fromList [("t3",2)]}),
          ("t1",TransitionNode {flowIn = M.empty, flowOut = M.fromList [("s1",1),("s3",1),("s4",1)]}),
          ("t2",TransitionNode {flowIn = M.fromList [("s2",1)], flowOut = M.fromList [("s1",1)]}),
          ("t3",TransitionNode {flowIn = M.fromList [("s4",2)], flowOut = M.empty})]
        },
      DrawSettings {
        withPlaceNames = False,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      ))),
    (2,(True,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",PlaceNode {initial = 1, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.empty}),
          ("s2",PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.fromList [("t1",1),("t2",1)]}),
          ("s3",PlaceNode {initial = 0, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.empty}),
          ("s4",PlaceNode {initial = 0, flowIn = M.fromList [("t1",1)], flowOut = M.fromList [("t3",2)]}),
          ("t1",TransitionNode {flowIn = M.fromList [("s2",1)], flowOut = M.fromList [("s1",1),("s3",1),("s4",1)]}),
          ("t2",TransitionNode {flowIn = M.fromList [("s2",1)], flowOut = M.fromList [("s1",1),("s3",1)]}),
          ("t3",TransitionNode {flowIn = M.fromList [("s4",2)], flowOut = M.empty})]
        },
      DrawSettings {
        withPlaceNames = False,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      )))
    ],
  showSolution = False
  }

defaultFindConflictInstance :: FindInstance Conflict
defaultFindConflictInstance = FindInstance {
  drawFindWith = DrawSettings {
    withPlaceNames = False,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = Conflict {
    conflictTrans = (Transition 1,Transition 3),
    conflictPlaces = [Place 4]
    },
  net = PetriLike {
    allNodes = M.fromList [
      ("s1",PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.fromList [("t1",1)]}),
      ("s2",PlaceNode {initial = 0, flowIn = M.fromList [("t1",2)], flowOut = M.empty}),
      ("s3",PlaceNode {initial = 0, flowIn = M.fromList [("t1",2),("t2",1),("t3",1)], flowOut = M.empty}),
      ("s4",PlaceNode {initial = 1, flowIn = M.empty, flowOut = M.fromList [("t1",1),("t3",1)]}),
      ("t1",TransitionNode {flowIn = M.fromList [("s1",1),("s4",1)], flowOut = M.fromList [("s2",2),("s3",2)]}),
      ("t2",TransitionNode {flowIn = M.empty, flowOut = M.fromList [("s3",1)]}),
      ("t3",TransitionNode {flowIn = M.fromList [("s4",1)], flowOut = M.fromList [("s3",1)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
