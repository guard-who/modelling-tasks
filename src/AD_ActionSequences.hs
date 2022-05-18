module AD_ActionSequences (
  validActionSequence,
  generateActionSequence,
) where

import qualified Data.Set as S (fromList)
import qualified Data.Map as M (filter, map, keys, fromList, toList, null)

import AD_Datatype (
  ADNode(..),
  UMLActivityDiagram (..),
  isActionNode
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

import Modelling.PetriNet.Reach.Step (levels', successors)

import Control.Monad (guard)
import Data.List (find, union)
import Data.Maybe(mapMaybe, isJust, fromJust)


fromPetriLike :: (Ord a) => PetriLike a -> Net a a
fromPetriLike petri = 
  Net {
      places = S.fromList $ M.keys $ M.filter isPlaceNode $ allNodes petri,
      transitions = S.fromList $ M.keys $ M.filter isTransitionNode $ allNodes petri,
      connections = map (\(t,n) -> (M.keys $ flowIn n, t, M.keys $ flowOut n)) $ M.toList $ M.filter isTransitionNode $ allNodes petri,
      capacity = Unbounded,
      start = State {unState = M.map initial $ M.filter isPlaceNode $ allNodes petri}
  }

--Generate one valid action sequence to each of the final nodes
generateActionSequence :: UMLActivityDiagram -> [String]
generateActionSequence diag =
  let tSeq = generateActionSequence' diag 
      tSeqLabels = map (label :: PetriKey -> Int) tSeq
      actions = map (\n -> ((label :: ADNode -> Int) n, name n)) $ filter isActionNode $ nodes diag
  in mapMaybe (`lookup` actions) tSeqLabels
 
--Generate at one sequence of transitions to each final node
generateActionSequence' :: UMLActivityDiagram -> [PetriKey]
generateActionSequence' diag =
  let petri =  fromPetriLike $ convertToPetrinet diag
      zeroState = State $ M.map (const 0) $ unState $ start petri
      sequences = fromJust $ find (isJust . lookup zeroState) $ levels' petri 
  in reverse $ fromJust $ lookup zeroState sequences


validActionSequence :: [String] -> UMLActivityDiagram -> String
validActionSequence input diag =
  let nameMap = map (\n -> (name n, (label :: ADNode -> Int) n)) $ filter isActionNode $ nodes diag
      labels = mapMaybe (`lookup` nameMap) input  
      petri = convertToPetrinet diag
      petriKeyMap = map (\k -> ((label :: PetriKey -> Int) k, k)) $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) labels
      actions = map snd $ filter (\(l,_) -> l `elem` map snd nameMap) $ petriKeyMap
  in show (length input == length labels) ++ show(validActionSequence' input' actions petri) ++ show labels ++ show input' ++ show actions

validActionSequence' :: [PetriKey] -> [PetriKey] -> PetriLike PetriKey -> Bool
validActionSequence' input actions petri =
  let net = fromPetriLike $ petri
      zeroState = State $ M.map (const 0) $ unState $ start net
      finals = filter (\k -> not $ k `elem` actions) $ M.keys $ M.filter (M.null . flowOut) $ allNodes petri 
  in not . null $ filter (isJust . lookup zeroState) $ levelsCheckAS input actions finals net


levelsCheckAS :: [PetriKey] -> [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey-> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions finals n =
  let f _ [] = []
      f [] xs =
        let next = M.toList $
              M.fromList $ do
                (x, p) <- xs
                (t, y) <- successors n x
                guard $ not $ t `elem` actions        -- No futher actions should be processed if no input is left
                return (y, t : p)
        in xs : f [] next
      f (a:as) xs =
        let consume = M.toList $
              M.fromList $ do
                (x, p) <- xs
                (t, y) <- successors n x
                guard $ t == a                     -- Case: Next transitions correspond to next input, therefore is processed and removed
                return (y, t : p)
            notConsume = M.toList $
              M.fromList $ do
                (x, p) <- xs
                (t, y) <- successors n x
                guard $ not $ t `elem` actions      --Case: Next transition is not an action, therefore is processed but not removed from input
                guard $ not $ t `elem` finals       --Supress firing of transitions representing final nodes until input is processed
                return (y, t : p)
         in union (xs : f as consume) (xs : f (a:as) notConsume)
  in f input [(start n, [])]





{--
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
--}