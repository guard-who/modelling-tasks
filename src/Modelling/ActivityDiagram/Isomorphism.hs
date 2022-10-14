{-# LANGUAGE FlexibleContexts #-}
module Modelling.ActivityDiagram.Isomorphism (
  isADIsomorphic,
  isPetriIsomorphic
) where

import qualified Data.Map as M (keys)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram)
import Modelling.ActivityDiagram.Petrinet (convertToSimple)
import Modelling.PetriNet.Types (Net (..))
import Data.Graph (Graph, graphFromEdges')
import Data.Graph.Automorphism (isIsomorphic)

isADIsomorphic :: UMLActivityDiagram -> UMLActivityDiagram -> Bool
isADIsomorphic ad1 ad2 =
  isPetriIsomorphic (convertToSimple ad1) (convertToSimple ad2)

isPetriIsomorphic
  :: (Net p n, Net p' n', Ord a)
  => p n a
  -> p' n' a
  -> Bool
isPetriIsomorphic p1 p2 =
  isIsomorphic (petriToGraph p1) (petriToGraph p2)

petriToGraph :: (Net p n, Ord a) => p n a -> Graph
petriToGraph petri =
  let keys = M.keys $ nodes petri
      keyToEdgeList k = M.keys $ outFlow k petri
  in fst $ graphFromEdges' $ map (\k -> (k, k, keyToEdgeList k)) keys
