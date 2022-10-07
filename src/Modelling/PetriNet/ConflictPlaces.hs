{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Modelling.PetriNet.ConflictPlaces where

import qualified Data.Map                         as M (empty, fromList)

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
  BasicConfig (..),
  Conflict,
  DrawSettings (..),
  FindConflictConfig (..),
  PetriConflict (..),
  PetriLike (..),
  SimpleNode (..),
  defaultFindConflictConfig,
  lBasicConfig,
  lHidePlaceNames,
  )

import Control.Applicative              ((<|>))
import Control.Lens                     ((.~))
import Control.Monad                    (forM_, void)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Output (
  LangM',
  LangM,
  OutputMonad (..),
  continueOrAbort,
  english,
  german,
  translate,
  )
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Function                    ((&))
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

findConflictPlacesTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance Conflict
  -> LangM m
findConflictPlacesTask path task = do
  pn <- renderWith path "conflict" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english "Considering this Petri net"
    german "Betrachten Sie folgendes Petrinetz"
  image pn
  paragraph $ translate $ do
    english "Which pair of transitions are in conflict because of which place(s) under the initial marking?"
    german "Welches Paar von Transitionen steht wegen welcher konfliktauslösenden Stelle(n) unter der Startmarkierung in Konflikt?"
  paragraph $ do
    translate $ do
      english "Please state your answer by giving a pair of conflicting transitions and the list of all the places that induce the conflict. "
      german "Geben Sie Ihre Antwort durch Angabe eines Paars von in Konflikt stehenden Transitionen und die Liste aller Stellen, die den Konflikt auslösen, an. "
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
and that places #{p1} and #{p2} are all the common places within the preconditions
which each do not have enough tokens to fire #{t1} and #{t2} at the same time. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung in Konflikt stehen
und dass die Stellen #{p1} und #{p2} alle gemeinsamen Stellen in den Vorbedingungen sind,
die jeweils nicht ausreichend Marken zum gleichzeitigen Feuern der Transitionen #{t1} und #{t2} haben. |]
    translate $ do
      english [i|The order of transitions within the pair does not matter here.
The order of places within the lisiting of places inducing the conflict is irrelevant as well.|]
      german [i|Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle.
Die Reihenfolge von Stellen innerhalb der Auflistung der den Konflikt auslösenden Stellen spielt ebenso keine Rolle.|]
  paragraph hoveringInformation

conflictInitial :: ConflictPlaces
conflictInitial = (findInitial, [Place 0, Place 1])

findConflictPlacesSyntax
  :: OutputMonad m
  => FindInstance Conflict
  -> ConflictPlaces
  -> LangM' m ()
findConflictPlacesSyntax task (conflict, ps) = do
  findConflictSyntax task conflict
  forM_ ps $ \x -> assert (isValidPlace x) $ translate $ do
    let x' = show $ ShowPlace x
    english $ x' ++ " is a valid place of the given Petri net?"
    german $ x' ++ " ist eine gültige Stelle des gegebenen Petrinetzes?"
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
  & lBasicConfig . lHidePlaceNames .~ False

checkFindConflictPlacesConfig :: FindConflictConfig -> Maybe String
checkFindConflictPlacesConfig FindConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig
  }
  = prohibitHidePlaceNames basicConfig
  <|> checkConfigForFind basicConfig changeConfig
  <|> checkConflictConfig basicConfig conflictConfig

prohibitHidePlaceNames :: BasicConfig -> Maybe String
prohibitHidePlaceNames bc
  | hidePlaceNames bc
  = Just "Place names are required for this task type."
  | otherwise
  = Nothing

defaultFindConflictPlacesInstance :: FindInstance Conflict
defaultFindConflictPlacesInstance = FindInstance {
  drawFindWith = DrawSettings {
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
