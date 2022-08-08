module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  generateActionSequence,
) where

import qualified Data.Set as S (fromList)
import qualified Data.Map as M (filter, map, keys, fromList, toList)

import Modelling.ActivityDiagram.Datatype (
  ADNode(..),
  UMLActivityDiagram (..),
  isActionNode
  )

import Modelling.ActivityDiagram.Petrinet (
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


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let nameMap = map (\n -> (name n, (label :: ADNode -> Int) n)) $ filter isActionNode $ nodes diag
      labels = mapMaybe (`lookup` nameMap) input
      petri = convertToPetrinet diag
      petriKeyMap = map (\k -> ((label :: PetriKey -> Int) k, k)) $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) labels
      actions = map snd $ filter (\(l,_) -> l `elem` map snd nameMap) petriKeyMap
  in length input == length labels && validActionSequence' input' actions petri


validActionSequence' :: [PetriKey] -> [PetriKey] -> PetriLike PetriKey -> Bool
validActionSequence' input actions petri =
  let net = fromPetriLike petri
      zeroState = State $ M.map (const 0) $ unState $ start net
  in any (isJust . lookup zeroState) (levelsCheckAS input actions net)


levelsCheckAS :: [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey-> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions n =
  let g h xs = M.toList $
        M.fromList $ do
          (x, p) <- xs
          (t, y) <- successors n x
          guard $ h t
          return (y, t : p)
      f _ [] = []
      f [] xs =
        let next = g (`notElem` actions) xs               -- No further actions should be processed if no input is left
        in xs : f [] next
      f (a:as) xs =
        let consume = g (==a) xs                          -- Case: Next transition corresponds to input, therefore is processed and removed
            notConsume = g (`notElem` actions) xs         -- Case: Next transition is not an action, therefore is processed but not removed from input
        in union (f as consume) (f (a:as) notConsume)
  in f input [(start n, [])]