module AD_Isomorphism (
  isADIsomorphic,
  isPetriIsomorphic
) where

import qualified Data.Map as M ((!), keys)

import AD_Datatype (UMLActivityDiagram)
import AD_Petrinet (convertToPetrinet)
import Modelling.PetriNet.Types (PetriLike(..), Node(..))
import Data.Graph (Graph, graphFromEdges')
import Data.Graph.Automorphism (isIsomorphic)

isADIsomorphic :: UMLActivityDiagram -> UMLActivityDiagram -> Bool
isADIsomorphic ad1 ad2 =
  isPetriIsomorphic (convertToPetrinet ad1) (convertToPetrinet ad2)

isPetriIsomorphic :: (Ord a) => PetriLike a -> PetriLike a -> Bool
isPetriIsomorphic p1 p2 =
  isIsomorphic (petriToGraph p1) (petriToGraph p2)

petriToGraph :: (Ord a) => PetriLike a -> Graph
petriToGraph petri =
  let keys = M.keys $ allNodes petri
      keyToEdgeList k = M.keys $ flowOut $ allNodes petri M.! k
  in fst $ graphFromEdges' $ map (\k -> (k, k, keyToEdgeList k)) keys
