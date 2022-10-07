{-# LANGUAGE FlexibleContexts #-}
module Modelling.ActivityDiagram.Isomorphism (
  isADIsomorphic,
  isPetriIsomorphic
) where

import qualified Data.Map as M (keys)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram)
import Modelling.ActivityDiagram.Petrinet (convertToPetrinet)
import Modelling.PetriNet.Types (Net (..), PetriLike (..))
import Data.Graph (Graph, graphFromEdges')
import Data.Graph.Automorphism (isIsomorphic)

isADIsomorphic :: UMLActivityDiagram -> UMLActivityDiagram -> Bool
isADIsomorphic ad1 ad2 =
  isPetriIsomorphic (convertToPetrinet ad1) (convertToPetrinet ad2)

isPetriIsomorphic
  :: (Net PetriLike n, Ord a)
  => PetriLike n a
  -> PetriLike n a
  -> Bool
isPetriIsomorphic p1 p2 =
  isIsomorphic (petriToGraph p1) (petriToGraph p2)

petriToGraph :: (Net PetriLike n, Ord a) => PetriLike n a -> Graph
petriToGraph petri =
  let keys = M.keys $ allNodes petri
      keyToEdgeList k = M.keys $ outFlow k petri
  in fst $ graphFromEdges' $ map (\k -> (k, k, keyToEdgeList k)) keys
