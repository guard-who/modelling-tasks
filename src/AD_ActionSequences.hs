module AD_ActionSequences (
  validActionSequence,
  generateActionSequence,
) where

import qualified Data.Set as S (fromList)
import qualified Data.Map as M (filter, map, keys, toList, null)

import AD_Datatype (
  ADNode(..),
  UMLActivityDiagram (..),
  adjNodes, getInitialNodes, isActionNode
  )

import AD_Petrinet (
  PetriKey(..),
  convertToPetrinet 
  )

import Modelling.PetriNet.Types (
  PetriLike(..),
  Node(..),
  isPlaceNode, isTransitionNode
  )

import Modelling.PetriNet.Reach.Type (
  State(..),
  Capacity(..),
  Net(..)
  )

import Modelling.PetriNet.Reach.Step (levels')

import Data.List (delete, find)


fromPetriLike :: (Ord a) => PetriLike a -> Net a a
fromPetriLike petri = 
  Net {
      places = S.fromList $ M.keys $ M.filter isPlaceNode $ allNodes petri,
      transitions = S.fromList $ M.keys $ M.filter isTransitionNode $ allNodes petri,
      connections = map (\(t,n) -> (M.keys $ flowIn n, t, M.keys $ flowOut n)) $ M.toList $ M.filter isTransitionNode $ allNodes petri,
      capacity = Unbounded,
      start = State {unState = M.map initial $ M.filter isPlaceNode $ allNodes petri}
  }

--Generate a single valid action sequence to one of the final nodes
generateActionSequence :: UMLActivityDiagram -> Maybe [String]
generateActionSequence diag =
  case generateActionSequence' diag of
    Just (t:_) -> Just $ map name $ filter (\n -> isActionNode n && (label :: ADNode -> Int) n `elem` (map (label :: PetriKey -> Int) t)) $ nodes diag
    _ -> Nothing

--Generate at least one sequence of transitions to a final node
generateActionSequence' :: UMLActivityDiagram -> Maybe [[PetriKey]]
generateActionSequence' diag =
  let petriLike =  convertToPetrinet diag
      finals = M.keys $ M.filter (M.null . flowOut) $ allNodes petriLike
      sequences = find (not . null . filterSequences finals) $ levels' $ fromPetriLike petriLike
  in (map reverse) <$> filterSequences finals <$> sequences
  where 
    p ts = \xs -> not . null $ filter (\t -> t `elem` xs) ts 
    filterSequences ts = filter (p ts) . map snd



--To be reworked
validActionSequence :: [ADNode] -> UMLActivityDiagram -> Bool
validActionSequence input diag = all isActionNode input && validActionSequence' input diag (getInitialNodes diag)

validActionSequence' :: [ADNode] -> UMLActivityDiagram -> [ADNode] -> Bool
validActionSequence' [] _ [] = False
validActionSequence' [] diag (y:nextNodes) =                                                                 --Case: No input left
  case y of
          ADDecisionNode {} -> validActionSequence' [] diag (nextNodes ++ init (adjNodes y diag)) ||         --Decision node needs alternative handling
                               validActionSequence' [] diag (nextNodes ++ tail (adjNodes y diag))
          ADActionNode {} -> False                                                                           --Action node not in input
          ADActivityFinalNode {} -> True                                                                     --All input processed and activity final reached -> valid sequence
          _ -> validActionSequence' [] diag (nextNodes ++ adjNodes y diag)
validActionSequence' (_:_) _ [] = False                                                                      --No nodes left to traverse
validActionSequence' (x:input) diag (y:nextNodes) =                                                          
  if x `notElem` (y:nextNodes) then                                                                          --Case: Input not in next nodes to be traversed
    case y of
      ADDecisionNode {} -> validActionSequence' (x:input) diag (nextNodes ++ init (adjNodes y diag)) ||
                           validActionSequence' (x:input) diag (nextNodes ++ tail (adjNodes y diag))
      ADActionNode {} -> False                                                                               --Action node traversed thats not in input
      ADActivityFinalNode {} -> False                                                                        --Activity Final traversed with input left
      _ -> validActionSequence' (x:input) diag (nextNodes ++ adjNodes y diag)
  else validActionSequence' input diag (delete x (y:nextNodes))                                              --If action node is in nextNodes, remove from input

