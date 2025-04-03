{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  addPretext,
  checkTaskText,
  directionsAdvice,
  extra,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  ) where

import qualified Data.Map                         as M (empty, insert)

import Control.Monad.State (put)
import Control.OutputCapable.Blocks     (
  GenericOutputCapable (paragraph),
  Language,
  LangM,
  LangM',
  OutputCapable,
  english,
  german,
  translate,
  )
import Control.OutputCapable.Blocks.Type (
  SpecialOutput,
  checkTranslations,
  withRefusal,
  )
import Data.List                        ((\\), singleton)
import Data.Map                         (Map)
import Data.String.Interpolate          (iii)

hoveringInformation :: OutputCapable m => LangM m
hoveringInformation = translate $ do
  english [iii|
    Please note: When hovering over or clicking on edges / nodes or their
    labels, the respective components that belong together are highlighted.
    |]
  german [iii|
    Bitte beachten Sie: Beim Bewegen über oder Klicken auf
    Kanten / Knoten bzw. ihre Beschriftungen
    werden die jeweils zusammengehörenden Komponenten hervorgehoben.
    |]

directionsAdvice :: OutputCapable m => LangM m
directionsAdvice = translate $ do
  english [iii|
    As navigation directions are used,
    please note that aggregations and compositions are only navigable
    from the "part" toward the "whole",
    i.e., they are not navigable in the opposite direction!
    |]
  german [iii|
    Da Navigationsrichtungen verwendet werden, beachten Sie bitte,
    dass Aggregationen und Kompositionen
    nur vom "Teil" zum "Ganzen" navigierbar sind,
    d.h., sie sind nicht in der entgegengesetzten Richtung navigierbar!
    |]

simplifiedInformation :: OutputCapable m => LangM m
simplifiedInformation = translate $ do
  english [iii|
    Please note: Classes are represented simplified here.
    #{endLine}
    That means they consist of a single box containing only the class name
    but no sections for attributes or methods.
    #{endLine}
    Nevertheless you should treat these simplified class representations
    as valid classes.
    |]
  german [iii|
    Bitte beachten Sie: Klassen werden hier vereinfacht dargestellt.
    #{endLine}
    Das heißt, sie bestehen aus einer einfachen Box,
    die nur den Klassennamen enthält,
    aber keine Abschnitte für Attribute oder Methoden.
    #{endLine}
    Trotzdem sollten Sie diese vereinfachten Klassendarstellungen
    als valide Klassen ansehen.
    |]
  where
    endLine :: String
    endLine = "\n"

addPretext :: OutputCapable m => LangM' m a -> LangM' m a
addPretext = (*>) $
  paragraph $ translate $ do
    english "Remarks on your solution:"
    german "Anmerkungen zur eingereichten Lösung:"

uniform :: a -> Map Language a
uniform x = foldr (`M.insert` x) M.empty [minBound ..]

checkTaskText
  :: (Bounded element, Enum element, Eq element, Show element)
  => [SpecialOutput element]
  -> Maybe String
checkTaskText taskText
  | x:_ <- allElements \\ usedElements
  = Just [iii|Your task text is incomplete as it is missing '#{show x}'.|]
  | x:_ <- usedElements \\ allElements
  = Just [iii|
      Your task text is using '#{show x}' at least twice,
      but it should appear exactly once.
      |]
  | x:_ <- concatMap (checkTranslations (const [])) taskText
  = Just $ [iii|Problem within your task text: |] ++ x
  | any (withRefusal (const False)) taskText
  = Just [iii|
    Your task text must not refuse output! (i.e. use Refuse or Assertion)
    |]
  | otherwise
  = Nothing
  where
    usedElements = concatMap (concatMap singleton) taskText
    allElements = [minBound ..]

extra :: OutputCapable m => Maybe (Map Language String) -> LangM m
extra (Just extraMap) = paragraph $ translate $ put extraMap
extra _ = pure ()
