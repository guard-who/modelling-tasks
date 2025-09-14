{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice
  ) where

import Data.String.Interpolate          (iii)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  )

finalNodesAdvice :: OutputCapable m => Bool -> LangM m
finalNodesAdvice withFinalTransitionAdvice = do
  paragraph $ translate $ do
    english $ [iii|
      Hint on the translation to a Petri net:
      For final nodes no additional places are introduced.
      They are realised in a way that a token is consumed,
      i.e. disappears from the net at that position.
      |]
      `appendExtendedAdvice`
      [iii|
      If an additional transition is required to realise this behavior
      at a position in the diagram where there is a final node,
      this transition does not count as auxiliary node.
      |]
    german $ [iii|
      Hinweis zur Übersetzung in ein Petrinetz:
      Für Endknoten  werden keine zusätzlichen Stellen eingeführt.
      Sie werden so realisiert, dass ein Token verbraucht wird,
      also an dieser Position aus dem Netz verschwindet.
      |]
      `appendExtendedAdvice`
      [iii|
      Falls eine zusätzliche Transition erforderlich ist,
      um dieses Verhalten an einer Position im Diagramm zu realisieren,
      an der sich ein Endknoten befindet,
      zählt diese Transition nicht als Hilfsknoten.
      |]
  pure ()
  where
    appendExtendedAdvice x y
      | withFinalTransitionAdvice = x ++ ' ' : y
      | otherwise = x
