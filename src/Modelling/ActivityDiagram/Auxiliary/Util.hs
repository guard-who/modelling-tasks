{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice,
  checkCount
  ) where

import qualified Data.Map as M (size)
import qualified Modelling.PetriNet.Types as Petri (Net (nodes))

import Data.String.Interpolate          (iii)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  )
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram)
import Modelling.ActivityDiagram.PetriNet (convertToPetriNet)
import Modelling.PetriNet.Types (
  PetriLike,
  SimpleNode,
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

-- | Check if the count of Petri nodes in a converted activity diagram
-- falls within the given bounds
checkCount :: (Int, Maybe Int) -> UMLActivityDiagram -> Bool
checkCount countOfPetriNodesBounds ad =
  let count = M.size . Petri.nodes @PetriLike @SimpleNode
              $ convertToPetriNet ad
  in fst countOfPetriNodesBounds <= count
     && maybe True (count <=) (snd countOfPetriNodesBounds)
