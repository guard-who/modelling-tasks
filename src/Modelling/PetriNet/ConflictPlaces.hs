{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Modelling.PetriNet.ConflictPlaces (
  checkFindConflictPlacesConfig,
  conflictInitial,
  defaultFindConflictPlacesConfig,
  defaultFindConflictPlacesInstance,
  findConflictPlacesSyntax,
  findConflictPlacesTask,
  parseConflictPlacesPrec,
  simpleFindConflictPlacesTask,
  ) where

import qualified Data.Map                         as M (empty, fromList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Conflict (
  ConflictPlaces,
  checkConflictConfig,
  conflictPlacesShow,
  findConflictSyntax,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  checkConfigForFind,
  drawFindWith,
  findInitial,
  )
import Modelling.PetriNet.Diagram (
  renderWith,
  )
import Modelling.PetriNet.Reach.Type (
  Place (Place),
  ShowPlace (ShowPlace),
  Transition (Transition),
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types (
  Conflict,
  DrawSettings (..),
  FindConflictConfig (..),
  GraphConfig (..),
  Net,
  PetriConflict (..),
  PetriLike (..),
  SimpleNode (..),
  SimplePetriNet,
  defaultFindConflictConfig,
  lGraphConfig,
  lHidePlaceNames,
  )

import Control.Applicative              ((<|>))
import Control.Lens                     ((.~))
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM',
  LangM,
  OutputCapable,
  ($=<<),
  continueOrAbort,
  english,
  german,
  translate,
  )
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Function                    ((&))
import Data.Foldable                    (for_)
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.String.Interpolate          (i)
import Text.Parsec (
  char,
  endBy1,
  optionMaybe,
  optional,
  spaces,
  )
import Text.Parsec.String               (Parser)

simpleFindConflictPlacesTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> FindInstance SimplePetriNet Conflict
  -> LangM m
simpleFindConflictPlacesTask = findConflictPlacesTask

findConflictPlacesTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> FindInstance (p n String) Conflict
  -> LangM m
findConflictPlacesTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net:"
    german "Betrachten Sie folgendes Petrinetz:"
  image $=<< renderWith path "conflict" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english "Which pair of transitions is in conflict, and because of which conflict-causing place(s), under the initial marking?"
    german "Welches Paar von Transitionen steht in Konflikt, und wegen welcher konfliktauslösenden Stelle(n), unter der Startmarkierung?"
  paragraph $ do
    translate $ do
      english "Please state your answer by giving a pair of conflicting transitions and a list of all the places that induce the conflict. "
      german "Geben Sie Ihre Antwort durch Angabe eines Paars von in Konflikt stehenden Transitionen und einer Liste aller Stellen, die den Konflikt auslösen. "
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = conflictPlacesShow conflictInitial
    code $ show ts
    translate $ do
      let ((t1, t2), [p1, p2]) = bimap
            (bimap show show)
            (fmap show)
            ts
      english [i| as answer would indicate that transitions #{t1} and #{t2} are in conflict under the initial marking
and that places #{p1} and #{p2} are all those common places within the preconditions
which each separately do not have enough tokens for firing #{t1} and #{t2} at the same time. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung in Konflikt stehen
und dass die Stellen #{p1} und #{p2} all jene gemeinsamen Stellen in den Vorbedingungen sind,
die jeweils einzeln nicht ausreichend Marken zum gleichzeitigen Feuern der Transitionen #{t1} und #{t2} haben. |]
    translate $ do
      english [i|The order of transitions within the firstly given pair does not matter here.
The order of places within the listing of places inducing the conflict is irrelevant as well.|]
      german [i|Die Reihenfolge der Transitionen innerhalb des zuerst angegebenen Paars spielt hierbei keine Rolle.
Die Reihenfolge von Stellen innerhalb der Auflistung der den Konflikt auslösenden Stellen spielt ebenso keine Rolle.|]
    pure ()
  paragraph hoveringInformation
  pure ()

conflictInitial :: ConflictPlaces
conflictInitial = (findInitial, [Place 0, Place 1])

findConflictPlacesSyntax
  :: OutputCapable m
  => FindInstance net Conflict
  -> ConflictPlaces
  -> LangM' m ()
findConflictPlacesSyntax task (conflict, ps) = do
  findConflictSyntax task conflict
  for_ ps $ \x -> assert (isValidPlace x) $ translate $ do
    let x' = show $ ShowPlace x
    english $ x' ++ " is a valid place of the given Petri net?"
    german $ x' ++ " ist eine gültige Stelle des gegebenen Petrinetzes?"
  pure ()
  where
    isValidPlace (Place x) = x >= 1 && x <= numberOfPlaces task
    assert = continueOrAbort $ showSolution task

parseConflictPlacesPrec :: Int -> Parser ConflictPlaces
parseConflictPlacesPrec _  = do
  spaces
  mo <- optionMaybe (char '(')
  x <- Left <$> parseConflict mo
    <|> Right <$> parsePlaces
  spaces
  optional (char ',')
  y <- either (\y -> (y,) <$> parsePlaces) (\y -> (,y) <$> parseConflict Nothing) x
  spaces
  optional (char ')')
  spaces
  return y
  where
    parseConflict mo = do
      spaces
      maybe void (const optional) mo $ char '('
      t1 <- parseTransitionPrec 0
      spaces
      void $ char ','
      t2 <- parseTransitionPrec 0
      spaces
      void $ char ')'
      return (t1, t2)
    parsePlaces =
      spaces
      *> char '['
      *> parsePlacePrec 0 `endBy1` (spaces <* optional (char ','))
      <*  char ']'

defaultFindConflictPlacesConfig :: FindConflictConfig
defaultFindConflictPlacesConfig = defaultFindConflictConfig
  & lGraphConfig . lHidePlaceNames .~ False

checkFindConflictPlacesConfig :: FindConflictConfig -> Maybe String
checkFindConflictPlacesConfig FindConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig,
  graphConfig
  }
  = prohibitHidePlaceNames graphConfig
  <|> checkConfigForFind basicConfig changeConfig graphConfig
  <|> checkConflictConfig basicConfig conflictConfig

prohibitHidePlaceNames :: GraphConfig -> Maybe String
prohibitHidePlaceNames gc
  | hidePlaceNames gc
  = Just "Place names are required for this task type."
  | otherwise
  = Nothing

defaultFindConflictPlacesInstance :: FindInstance SimplePetriNet Conflict
defaultFindConflictPlacesInstance = FindInstance {
  drawFindWith = DrawSettings {
    withAnnotatedLabels = False,
    withPlaceNames = True,
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
      ("s1",SimplePlace {initial = 1, flowOut = M.fromList [("t1",1)]}),
      ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
      ("s3",SimplePlace {initial = 0, flowOut = M.empty}),
      ("s4",SimplePlace {initial = 1, flowOut = M.fromList [("t1",1),("t3",1)]}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s2",2),("s3",2)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s3",1)]}),
      ("t3",SimpleTransition {flowOut = M.fromList [("s3",1)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
