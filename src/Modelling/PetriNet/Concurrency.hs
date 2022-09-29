{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig,
  checkPickConcurrencyConfig,
  defaultFindConcurrencyInstance,
  defaultPickConcurrencyInstance,
  findConcurrency,
  findConcurrencyEvaluation,
  findConcurrencyGenerate,
  findConcurrencySolution,
  findConcurrencySyntax,
  findConcurrencyTask,
  parseConcurrency,
  petriNetFindConcur,
  petriNetPickConcur,
  pickConcurrency,
  pickConcurrencyGenerate,
  pickConcurrencyTask,
  ) where

import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Modelling.Auxiliary.Common (
  Object,
  oneOf,
  parseWith,
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
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind,
  checkConfigForPick,
  )
import Modelling.PetriNet.Diagram (
  renderWith,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  findInitial,
  findTaskInstance,
  toFindEvaluation,
  toFindSyntax,
  )
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Pick (
  PickInstance (..),
  pickGenerate,
  pickTaskInstance,
  renderPick,
  wrong,
  wrongInstances,
  )
import Modelling.PetriNet.Reach.Type (
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  DrawSettings (..),
  FindConcurrencyConfig (..),
  Node (..),
  PetriLike (PetriLike, allNodes),
  PickConcurrencyConfig (..),
  transitionPairShow,
  )

import Control.Monad.Output (
  LangM',
  LangM,
  OutputMonad (..),
  Rated,
  english,
  german,
  printSolutionAndAssert,
  translate,
  translations,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Either                      (isLeft)
import Data.GraphViz.Commands           (GraphvizCommand (Circo, Fdp))
import Data.String.Interpolate          (i)
import Language.Alloy.Call (
  AlloyInstance,
  )


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
      german "Geben Sie Ihre Antwort durch Angabe eines Paars von nebenläufig aktivierten Transitionen an. "
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = transitionPairShow findInitial
    code $ show ts
    translate $ do
      let (t1, t2) = bimap show show ts
      english [i| as answer would indicate that transitions #{t1} and #{t2} are concurrently activated under the initial marking. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung nebenläufig aktiviert sind. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

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
        english "are concurrently activated"
        german "sind nebenläufig aktiviert"
  result <- toFindEvaluation what withSol concur x
  uncurry printSolutionAndAssert result
  where
    concur = findConcurrencySolution task
    withSol = showSolution (task :: FindInstance (Concurrent Transition))

findConcurrencySolution :: FindInstance (Concurrent a) -> (a, a)
findConcurrencySolution task = concur
  where
    Concurrent concur = toFind task

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
    german "Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzes an, das diese nebenläufig aktivierten Transitionen hat. "
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that Petri net 1 has exactly two transitions that are concurrently activated (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]
      german $ "als Antwort würde bedeuten, dass Petrinetz 1 genau zwei nebenläufig aktivierte Transitionen hat (und dass "
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


petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{
  basicConfig,
  advConfig,
  changeConfig
  } = petriNetConcurrencyAlloy basicConfig changeConfig $ Right advConfig

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur PickConcurrencyConfig{
  basicConfig,
  changeConfig,
  prohibitSourceTransitions
  } =
  petriNetConcurrencyAlloy
    basicConfig
    changeConfig
    (Left prohibitSourceTransitions)

{-|
Generate code for PetriNet concurrency tasks
-}
petriNetConcurrencyAlloy
  :: BasicConfig
  -> ChangeConfig
  -> Either Bool AdvConfig
  -- ^ Right for find task; Left for pick task
  -> String
petriNetConcurrencyAlloy basicC changeC specific
  = [i|module #{moduleName}

#{modulePetriSignature}
#{either (const sigs) (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{predicate}[#{place}#{defaultActivTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{sourceTransitionConstraints}
  #{constraints}
  #{compConstraints}
}

run #{predicate} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
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
    constraints :: String
    constraints = [i|
  no x,y : givenTransitions | x != y and concurrentDefault[x + y]
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
  all u,v : Transitions |
    u != v and concurrent[u + v] implies #{t1} + #{t2} = u + v|]
    defaultActivTrans
      | isLeft specific    = [i|#{activatedDefault} : set givenTransitions,|]
      | otherwise          = ""
    moduleName = "PetriNetConcur"
    place = ""
    predicate = concurrencyPredicateName
    sigs = signatures "given" (places basicC) (transitions basicC)
    t1 = transition1
    t2 = transition2

concurrencyPredicateName :: String
concurrencyPredicateName = "showConcurrency"

concurrencyTransition1 :: String
concurrencyTransition1 = skolemVariable concurrencyPredicateName transition1

concurrencyTransition2 :: String
concurrencyTransition2 = skolemVariable concurrencyPredicateName transition2

transition1 :: String
transition1 = "transition1"

transition2 :: String
transition2 = "transition2"

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

defaultPickConcurrencyInstance :: PickInstance
defaultPickConcurrencyInstance = PickInstance {
  nets = M.fromList [
    (1,(False,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",PlaceNode {initial = 1, flowIn = M.fromList [("t1",1)], flowOut = M.fromList [("t1",2),("t2",1),("t3",1)]}),
          ("s2",PlaceNode {initial = 0, flowIn = M.fromList [("t3",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.fromList [("t3",1)], flowOut = M.fromList [("t1",1)]}),
          ("s4",PlaceNode {initial = 1, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.empty}),
          ("t1",TransitionNode {flowIn = M.fromList [("s1",2),("s3",1)], flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",TransitionNode {flowIn = M.fromList [("s1",1)], flowOut = M.fromList [("s4",1)]}),
          ("t3",TransitionNode {flowIn = M.fromList [("s1",1)], flowOut = M.fromList [("s2",1),("s3",1)]})
          ]
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
          ("s1",PlaceNode {initial = 2, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.fromList [("t1",2),("t2",1),("t3",1)]}),
          ("s2",PlaceNode {initial = 0, flowIn = M.fromList [("t3",1)], flowOut = M.empty}),
          ("s3",PlaceNode {initial = 0, flowIn = M.fromList [("t3",1)], flowOut = M.fromList [("t1",1)]}),
          ("s4",PlaceNode {initial = 2, flowIn = M.fromList [("t1",1),("t2",1)], flowOut = M.fromList [("t2",1)]}),
          ("t1",TransitionNode {flowIn = M.fromList [("s1",2),("s3",1)], flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",TransitionNode {flowIn = M.fromList [("s1",1),("s4",1)], flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t3",TransitionNode {flowIn = M.fromList [("s1",1)], flowOut = M.fromList [("s2",1),("s3",1)]})
          ]
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

defaultFindConcurrencyInstance :: FindInstance (Concurrent Transition)
defaultFindConcurrencyInstance = FindInstance {
  drawFindWith = DrawSettings {
    withPlaceNames = False,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = Concurrent (Transition 1,Transition 3),
  net = PetriLike {
    allNodes = M.fromList [
      ("s1",PlaceNode {initial = 2, flowIn = M.empty, flowOut = M.fromList [("t1",1),("t2",2),("t3",1)]}),
      ("s2",PlaceNode {initial = 1, flowIn = M.fromList [("t2",1),("t3",2)], flowOut = M.empty}),
      ("s3",PlaceNode {initial = 1, flowIn = M.fromList [("t1",1)], flowOut = M.fromList [("t3",1)]}),
      ("s4",PlaceNode {initial = 0, flowIn = M.fromList [("t2",2)], flowOut = M.empty}),
      ("t1",TransitionNode {flowIn = M.fromList [("s1",1)], flowOut = M.fromList [("s3",1)]}),
      ("t2",TransitionNode {flowIn = M.fromList [("s1",2)], flowOut = M.fromList [("s2",1),("s4",2)]}),
      ("t3",TransitionNode {flowIn = M.fromList [("s1",1),("s3",1)], flowOut = M.fromList [("s2",2)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
