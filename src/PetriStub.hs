module PetriStub (
  Node(..),
  PetriLike(..)
) where 

import Data.Map (Map)

data Node a =
  PlaceNode {
  -- | initial tokens of a 'PlaceNode'
  initial :: Int,
  -- | successor nodes
  flowIn  :: Map a Int,
  -- | predecessor nodes
  flowOut :: Map a Int
  } |
  TransitionNode {
  flowIn  :: Map a Int,
  flowOut :: Map a Int
  }
  deriving (Show)


newtype PetriLike a = PetriLike {
  -- | the 'Map' of all 'Node's the Petri net like graph is made of
  allNodes :: Map a (Node a)
} deriving (Show)