{-# LANGUAGE ApplicativeDo #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}

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
  simpleFindConcurrencyTask,
  simplePickConcurrencyTask,
  ) where

import qualified Modelling.PetriNet.Find          as F (showSolution)
import qualified Modelling.PetriNet.Types         as Find (
  FindConcurrencyConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  PickConcurrencyConfig (..),
  )

import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
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
  petriScopeBitWidth,
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
  GraphConfig (..),
  Net (..),
  PetriLike (PetriLike, allNodes),
  PickConcurrencyConfig (..),
  SimpleNode (..),
  SimplePetriNet,
  transitionPairShow,
  )

import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM',
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  printSolutionAndAssert,
  translate,
  translations,
  unLangM,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Either                      (isLeft)
import Data.GraphViz.Commands           (GraphvizCommand (Circo, Fdp))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance,
  )

simpleFindConcurrencyTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> FindInstance SimplePetriNet (Concurrent Transition)
  -> LangM m
simpleFindConcurrencyTask = findConcurrencyTask

findConcurrencyTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> FindInstance (p n String) (Concurrent Transition)
  -> LangM m
findConcurrencyTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net:"
    german "Betrachten Sie folgendes Petrinetz:"
  image
    $=<< renderWith path "concurrent" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english [iii|
      Which pair of transitions are concurrently activated
      under the initial marking?
      |]
    german [iii|
      Welches Paar von Transitionen ist unter der Startmarkierung
      nebenläufig aktiviert?
      |]
  paragraph $ do
    translate $ do
      english [iii|
        Please state your answer by giving a pair
        of concurrently activated transitions.
        #{" "}|]
      german [iii|
        Geben Sie Ihre Antwort durch Angabe eines Paars
        von nebenläufig aktivierten Transitionen an.
        #{" "}|]
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = transitionPairShow findInitial
    code $ show ts
    translate $ do
      let (t1, t2) = bimap show show ts
      english [iii|
        #{" "}as answer would indicate that transitions #{t1} and #{t2}
        are concurrently activated under the initial marking.
        #{" "}|]
      german [iii|
        #{" "}als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2}
        unter der Startmarkierung nebenläufig aktiviert sind.
        #{" "}|]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german [iii|
        Die Reihenfolge der Transitionen innerhalb
        des Paars spielt hierbei keine Rolle.
        |]
    pure ()
  paragraph hoveringInformation
  pure ()

findConcurrencySyntax
  :: OutputCapable m
  => FindInstance net (Concurrent Transition)
  -> (Transition, Transition)
  -> LangM' m ()
findConcurrencySyntax task = toFindSyntax withSol $ numberOfTransitions task
  where
    withSol = F.showSolution task

findConcurrencyEvaluation
  :: (Monad m, OutputCapable m)
  => FindInstance net (Concurrent Transition)
  -> (Transition, Transition)
  -> Rated m
findConcurrencyEvaluation task x = do
  let what = translations $ do
        english "are concurrently activated"
        german "sind nebenläufig aktiviert"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ toFindEvaluation what withSol concur x
  where
    concur = findConcurrencySolution task
    withSol = F.showSolution task

findConcurrencySolution :: FindInstance net (Concurrent a) -> (a, a)
findConcurrencySolution task = concur
  where
    Concurrent concur = toFind task

simplePickConcurrencyTask
  :: (MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> PickInstance SimplePetriNet
  -> LangM m
simplePickConcurrencyTask = pickConcurrencyTask

pickConcurrencyTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> PickInstance (p n String)
  -> LangM m
pickConcurrencyTask path task = do
  paragraph $ translate $ do
    english [iii|
      Which of the following Petri nets has exactly
      one pair of transitions that are concurrently activated?
      |]
    german [iii|
      Welches dieser Petrinetze hat genau ein Paar von Transitionen,
      die nebenläufig aktiviert sind?
      |]
  images show snd
    $=<< renderPick path "concurrent" task
  paragraph $ translate $ do
    english [iii|
      Please state your answer by giving only the number of the Petri net
      having these concurrently activated transitions.
      #{" "}|]
    german [iii|
      Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzes an,
      das diese nebenläufig aktivierten Transitionen hat.
      #{" "}|]
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [iii|
        #{" "}as answer would indicate that Petri net 1 has
        exactly two transitions that are concurrently activated
        (and the other Petri
        #{if plural then "nets don't" else "net doesn't"}!).
        |]
      german $ [iii|
        #{" "}als Antwort würde bedeuten, dass Petrinetz 1
        genau zwei nebenläufig aktivierte Transitionen hat (und dass
        #{" "}
        |]
        ++ (if plural
            then "die anderen Petrinetze dies nicht tun"
            else "das andere Petrinetz dies nicht tut")
        ++ ")."
    pure ()
  paragraph hoveringInformation
  pure ()

findConcurrencyGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => FindConcurrencyConfig
  -> Int
  -> Int
  -> m (FindInstance (p n String) (Concurrent Transition))
findConcurrencyGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findConcurrency config segment
  gl <- oneOf $ graphLayouts gc
  c' <- lift $ traverse
     (parseWith parseTransitionPrec)
     c
  return $ FindInstance {
    drawFindWith   = DrawSettings {
      withAnnotatedLabels = False,
      withPlaceNames = not $ hidePlaceNames gc,
      withTransitionNames = not $ hideTransitionNames gc,
      with1Weights = not $ hideWeight1 gc,
      withGraphvizCommand = gl
      },
    toFind = c',
    net = d,
    numberOfPlaces = places bc,
    numberOfTransitions = transitions bc,
    showSolution = Find.printSolution config
    }
  where
    bc = Find.basicConfig config
    gc = Find.graphConfig config

findConcurrency
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => FindConcurrencyConfig
  -> Int
  -> RandT g m (p n String, Concurrent String)
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  Find.alloyConfig

pickConcurrencyGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => PickConcurrencyConfig
  -> Int
  -> Int
  -> m (PickInstance (p n String))
pickConcurrencyGenerate = pickGenerate pickConcurrency gc ud ws
  where
    gc = Pick.graphConfig
    ud = Pick.useDifferentGraphLayouts
    ws = Pick.printSolution


pickConcurrency
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => PickConcurrencyConfig
  -> Int
  -> RandT
    g
    m
    [(p n String, Maybe (Concurrent String))]
pickConcurrency = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  Pick.alloyConfig


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
  = [i|module PetriNetConcur

#{modulePetriSignature}
#{either (const sigs) (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{concurrencyPredicateName}[#{defaultActiveTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{sourceTransitionConstraints}
  no disj x,y : givenTransitions | concurrentDefault[x + y]
  disj[#{t1}, #{t2}] and concurrent[#{t1} + #{t2}]
  all disj u,v : Transitions |
    concurrent[u + v] implies #{t1} + #{t2} = u + v
  #{compConstraints}
}

run #{concurrencyPredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActiveTrans"
    compConstraints = either
      (const $ defaultConstraints activatedDefault basicC)
      compAdvConstraints
      specific
    sourceTransitionConstraints
      | Left True <- specific = [i|
  no t : givenTransitions | no givenPlaces.flow[t]
  no t : Transitions | sourceTransitions[t]|]
      | otherwise = ""
    defaultActiveTrans
      | isLeft specific    = [i|#{activatedDefault} : set givenTransitions,|]
      | otherwise          = ""
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
It throws an error instead if unexpected behaviour occurs.
-}
parseConcurrency :: MonadThrow m => AlloyInstance -> m (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

checkFindConcurrencyConfig :: FindConcurrencyConfig -> Maybe String
checkFindConcurrencyConfig FindConcurrencyConfig {
  basicConfig,
  changeConfig,
  graphConfig
  }
  = checkConfigForFind basicConfig changeConfig graphConfig

checkPickConcurrencyConfig :: PickConcurrencyConfig -> Maybe String
checkPickConcurrencyConfig PickConcurrencyConfig {
  basicConfig,
  changeConfig,
  graphConfig,
  useDifferentGraphLayouts
  }
  = checkConfigForPick
    useDifferentGraphLayouts
    wrong
    basicConfig
    changeConfig
    graphConfig

defaultPickConcurrencyInstance :: PickInstance SimplePetriNet
defaultPickConcurrencyInstance = PickInstance {
  nets = M.fromList [
    (1,(False,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.fromList [("t1",2),("t2",1),("t3",1)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = M.fromList [("t1",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = M.empty}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("s4",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = False,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      ))),
    (2,(True,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",2),("t2",1),("t3",1)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = M.fromList [("t1",1)]}),
          ("s4",SimplePlace {initial = 2, flowOut = M.fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = False,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      )))
    ],
  showSolution = False
  }

defaultFindConcurrencyInstance :: FindInstance SimplePetriNet (Concurrent Transition)
defaultFindConcurrencyInstance = FindInstance {
  drawFindWith = DrawSettings {
    withAnnotatedLabels = False,
    withPlaceNames = False,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = Concurrent (Transition 1,Transition 3),
  net = PetriLike {
    allNodes = M.fromList [
      ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",1),("t2",2),("t3",1)]}),
      ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
      ("s3",SimplePlace {initial = 1, flowOut = M.fromList [("t3",1)]}),
      ("s4",SimplePlace {initial = 0, flowOut = M.empty}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s3",1)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s2",1),("s4",2)]}),
      ("t3",SimpleTransition {flowOut = M.fromList [("s2",2)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
