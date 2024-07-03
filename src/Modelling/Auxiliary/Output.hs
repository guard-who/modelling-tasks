{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  reRefuse,
  simplifiedInformation,
  ) where

import Control.Applicative              (Alternative)
import Control.OutputCapable.Blocks     (
  GenericOutputCapable (paragraph, refuse),
  LangM,
  LangM',
  OutputCapable,
  Rated,
  english,
  german,
  recoverWith,
  translate,
  )
import Control.OutputCapable.Blocks.Generic (($>>), ($>>=))
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
    That means they consist of a single box containing only its class name,
    but do not contain boxes for attributes and methods.
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

{-
Append some remarks after some rating function.
But re-reject afterwards (if it was rejected by the rating function).
-}
reRefuse
  :: (Alternative m, Monad m, OutputCapable m)
  => Rated m
  -> LangM m
  -> Rated m
reRefuse xs ys =
  recoverWith (pure 0) xs
    $>>= \x -> ys
    $>> either (refuse (pure ()) *>) pure x
