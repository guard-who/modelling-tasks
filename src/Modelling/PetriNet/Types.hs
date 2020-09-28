{-# Language DeriveTraversable #-}
{-# Language DuplicateRecordFields #-}
{-|
This module provides types to represent Petri nets.

A Petri net is a mathematical modelling language.
It is used to describe distributed systems.
Another name for Petri net is place / transition (PT) net.

The 'Modelling.PetriNet.Types' module defines basic type class instances and
functions to work on and transform Petri net representations.
-}
module Modelling.PetriNet.Types where

import qualified Data.Map.Lazy                    as M (
  elems, empty, filter, foldrWithKey, insert, keysSet, lookup,
  mapKeys, member, null
  )
import qualified Data.Set                         as S (empty, union)

import Data.GraphViz.Attributes.Complete (GraphvizCommand (Neato))
import Data.Map.Lazy                    (Map)
import Data.Maybe                       (fromMaybe)

data Change = Change {tokenChange :: [(String,Int)],flowChange :: [(String,String,Int)]}
  deriving (Eq,Show)
  
data Conflict = Conflict{conflictTrans :: (String,String),conflictPlace :: String}
  deriving Show
  
type Concurrent a = (a, a)

{-|
A node is part of a petri like graph (see 'PetriLike').
Each node stores its predecessor and successor nodes together with their weight
in the fields 'flowIn' and 'flowOut' respectively.
Additionally 'PlaceNode's have a value of initial tokens.
-}
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
  deriving Show

{-|
Returns 'Just' the 'initial' tokens of the given 'Node', if it is a 'PlaceNode',
otherwise it returns 'Nothing'.
-}
maybeInitial :: Node a -> Maybe Int
maybeInitial n = case n of
  PlaceNode      {} -> Just $ initial n
  TransitionNode {} -> Nothing

{-|
This function acts like 'fmap' on other 'Functor's.

Note that 'Node' is not a true 'Functor' and thus 'mapNode' is not a true 'fmap'
because an 'Ord' instance is required for 'Node's first type parameter for
'mapNode' to work, furthermore (and that is the original reason), 'mapNode'
uses 'M.mapKeys' internally in order to apply the mapping. Thus, the user of
'mapNode' is responsible to ensure that the transformation preserves uniqueness
on the available keys used.
-}
mapNode :: Ord b => (a -> b) -> Node a -> Node b
mapNode f (PlaceNode s i o) =
  PlaceNode s (M.mapKeys f i) (M.mapKeys f o)
mapNode f (TransitionNode i o) =
  TransitionNode (M.mapKeys f i) (M.mapKeys f o)

{-|
This function acts like 'traverse' on 'Traversable'.

Not that 'Node' is not 'Traversable' itself as it requires an 'Ord' instance
for the result type within the 'Applicative' of its first argument, the
applicative lifting transformation function.
This behaviour occurs, because the traversal changes the keys of the underlying
'Map'.
Transformations on this map require a specific traversal 'traverseKeyMap'.

The user is responsible to ensure uniqueness of the keys after the traversal.
Note, that the order of values could also change if the transformation is not
order-preserving.
-}
traverseNode :: (Applicative f, Ord b) => (a -> f b) -> Node a -> f (Node b)
traverseNode f (PlaceNode s i o)    =
  PlaceNode s <$> traverseKeyMap f i <*> traverseKeyMap f o
traverseNode f (TransitionNode i o) =
  TransitionNode <$> traverseKeyMap f i <*> traverseKeyMap f o

{-|
A specific traversal for 'Map's changing the keys rather than values.
That is why, the result requires an 'Ord' instance.
It calls 'traverseKeyAndValueMap' but transforms only the keys.
-}
traverseKeyMap
  :: (Applicative f, Ord k2)
  => (k1 -> f k2) -- ^ transformation on keys
  -> Map k1 a
  -> f (Map k2 a)
traverseKeyMap f = traverseKeyAndValueMap f pure

{-|
A specific traversal for 'Map's transforming its keys and its values.
That is why, the result requires an 'Ord' instance on the resulting key values
type.
The traversal happens by inserting every changed key value pair into a new map.
-}
traverseKeyAndValueMap
  :: (Applicative f, Ord k2)
  => (k1 -> f k2) -- ^ transformation function on keys
  -> (a -> f b)   -- ^ transformation function on values
  -> Map k1 a
  -> f (Map k2 b)
traverseKeyAndValueMap f g =
  M.foldrWithKey insertApplicativeKeyValue (pure M.empty)
  where
    insertApplicativeKeyValue k x rs = M.insert <$> f k <*> g x <*> rs

{-|
Whether the 'Node' is a 'PlaceNode'.
-}
isPlaceNode :: Node a -> Bool
isPlaceNode PlaceNode {} = True
isPlaceNode _            = False

{-|
Whether the 'Node' is a 'TransitionNode'.
-}
isTransitionNode :: Node a -> Bool
isTransitionNode TransitionNode {} = True
isTransitionNode _                 = False

{-|
A petri like graph consists of 'Node's which might have connections between each
other.

The 'PetriLike' graph is a valid petri net only if

 * 'PlaceNode's are only successors of 'TransitionNode's
 * 'TransitionNode's are only successors of 'PlaceNode's
 * the initial marking is valid (i.e. all initial tokens are not negative)
 * every weight is greater than zero
-}
newtype PetriLike a = PetriLike {
  -- | the 'Map' of all 'Node's the Petri net like graph is made of
  allNodes :: Map a (Node a)
  } deriving Show

{-|
A 'Functor' like 'fmap' on 'PetriLike'.

Note that 'PetriLike' is not a true 'Functor' as it requires the resulting type
to be an instance of 'Ord', because it uses 'M.mapKeys' in order to apply the
mapping on internal keys.

Thus, the user of 'mapPetriLike' is responsible to preserve uniqueness of values
(otherwise values might be lost after applying the mapping). Furthermore, if the
transformation is not order-preserving, the order of keys within 'Map's might
be changed.
-}
mapPetriLike :: Ord b => (a -> b) -> PetriLike a -> PetriLike b
mapPetriLike f x = PetriLike $ M.mapKeys f $ mapNode f <$> allNodes x

{-|
A 'Traversable' like 'traverse' on 'PetriLike'.

Note that 'PetriLike' is not a true 'Traversable' as it requires the resulting
type to be an instance of 'Ord', because it uses 'traverseKeyAndValueMap' which
requires this constraint due to changing keys of 'Map's.

Thus, the user is responsible to preserve uniqueness of keys.
Furthermore, the order of keys might be changed if the transformation is not
order-preserving.
-}
traversePetriLike
  :: (Applicative f, Ord b)
  => (a -> f b)
  -> PetriLike a
  -> f (PetriLike b)
traversePetriLike f x =
  PetriLike <$> traverseKeyAndValueMap f (traverseNode f) (allNodes x)

{-|
Transform a 'PetriLike' graph into a 'Petri' net.
It first checks if the given Petri net like graph is indeed a valid Petri net
(see also 'PetriLike'),

* if it is, the Petri net like graph is transformed into a Petri net by
  eliminating references to names of places and transitions at all. Instead
  'initialMarking' is given by a list (where each position represents
  different places) and 'trans'itions are given by lists of token change
  (where, again, each position represents a different place, but the same
  index within 'initialMarking' and 'trans' represents the same place).
* if it is not, a message is returned indicating the reason why the given
  Petri net like graph is not a valid Petri net.
-}
petriLikeToPetri :: (Show a, Ord a) => PetriLike a -> Either String Petri
petriLikeToPetri p = do
  isValid
  return $ Petri {
    initialMarking = initial <$> M.elems ps,
    trans          =
      foldr ((:) . toChangeTuple) [] ts
    }
  where
    ps = M.filter isPlaceNode $ allNodes p
    ts = M.filter isTransitionNode $ allNodes p
    isValid
      | not (M.null $ M.filter ((< 0) . initial) ps)
      = Left "Invalid petri net: place with negative token number"
      | any (`M.member` ts) (allRelatedNodes ts)
      = Left "related nodes of TransitionNodes contain TranisitionNodes"
      | any (`M.member` ps) (allRelatedNodes ps)
      = Left "related nodes of PlaceNodes contain PlaceNodes"
      | any (any (<= 0) . flowIn) ts
      = Left "flow to a transition is zero or less"
      | any (any (<= 0) . flowOut) ts
      = Left "flow from a transition is zero or less"
      | otherwise
      = return ()
    toChangeTuple n = (toFlowList flowIn n, toFlowList flowOut n)
    toFlowList f n = M.foldrWithKey
      (\k _ xs -> fromMaybe 0 (M.lookup k $ f n) : xs)
      []
      ps
    relatedNodes n = M.keysSet (flowIn n) `S.union` M.keysSet (flowOut n)
    allRelatedNodes = foldr
      (S.union . relatedNodes)
      S.empty

type Marking = [Int]
type Transition = (Marking,Marking)

{-|
Stores a mathematical representation of a Petri net based on a five tuple.
-}
data PetriMath a = PetriMath {
  -- | the five tuple itself
  netMath            :: a,
  -- | the places (the first element of the five tuple)
  placesMath         :: a,
  -- | the transitions (the second element of the five tuple)
  transitionsMath    :: a,
  {- | the token change of each transition
       (the third and fourth element of the five tuple) -}
  tokenChangeMath    :: [(a, a)],
  -- | the initial marking (the fifth element of the five tuple)
  initialMarkingMath :: a
  } deriving (Foldable, Functor, Traversable)

data Petri = Petri
  { initialMarking :: Marking
  , trans :: [Transition]
  } deriving (Eq,Show)
  
defaultPetri :: Petri
defaultPetri = Petri
  { initialMarking = [1,1,0]
  , trans = [([1,0,0],[0,1,0]),([1,0,0],[0,0,1]),([0,1,1],[2,0,0])]
  }
  
placeHoldPetri :: Petri
placeHoldPetri = Petri{initialMarking =[],trans=[]}
  
data BasicConfig = BasicConfig
  { places :: Int
  , transitions :: Int
  , atLeastActive :: Int
  , minTokensOverall :: Int
  , maxTokensOverall :: Int
  , maxTokensPerPlace :: Int
  , minFlowOverall :: Int
  , maxFlowOverall :: Int
  , maxFlowPerEdge :: Int
  , graphLayout :: GraphvizCommand
  } deriving Show

defaultBasicConfig :: BasicConfig
defaultBasicConfig = BasicConfig
  { places = 4
  , transitions = 3
  , atLeastActive = 1
  , minTokensOverall = 2
  , maxTokensOverall = 7
  , maxTokensPerPlace = 2
  , minFlowOverall = 6
  , maxFlowOverall = 12
  , maxFlowPerEdge = 2
  , graphLayout = Neato
  }
  
data AdvConfig = AdvConfig
  { presenceOfSelfLoops :: Maybe Bool
  , presenceOfSinkTransitions :: Maybe Bool
  , presenceOfSourceTransitions :: Maybe Bool
  } deriving Show
  
defaultAdvConfig :: AdvConfig
defaultAdvConfig = AdvConfig
  { presenceOfSelfLoops = Just False
  , presenceOfSinkTransitions = Just False
  , presenceOfSourceTransitions = Just False
  }

data ChangeConfig = ChangeConfig
  { tokenChangeOverall :: Int
  , maxTokenChangePerPlace :: Int
  , flowChangeOverall :: Int
  , maxFlowChangePerEdge :: Int
  } deriving Show
  
defaultChangeConfig :: ChangeConfig
defaultChangeConfig = ChangeConfig
  { tokenChangeOverall = 2
  , maxTokenChangePerPlace = 1
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }
--------------------------------------------
data MathConfig = MathConfig 
  { basicTask :: BasicConfig
  , advTask :: AdvConfig
  , changeTask :: ChangeConfig
  } deriving Show
  
defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig
  { basicTask = defaultBasicConfig
  , advTask = defaultAdvConfig
  , changeTask = defaultChangeConfig{ tokenChangeOverall = 0, maxTokenChangePerPlace = 0 }
  }
  
data FindConflictConfig = FindConflictConfig
  { basicTask :: BasicConfig
  , advTask :: AdvConfig
  , changeTask :: ChangeConfig
  } deriving Show
  
defaultFindConflictConfig :: FindConflictConfig
defaultFindConflictConfig = FindConflictConfig
  { basicTask = defaultBasicConfig{ atLeastActive = 3 }
  , advTask = defaultAdvConfig{ presenceOfSourceTransitions = Nothing }
  , changeTask = defaultChangeConfig
  }
  
data PickConflictConfig = PickConflictConfig
  { basicTask :: BasicConfig
  , changeTask :: ChangeConfig
  } deriving Show

defaultPickConflictConfig :: PickConflictConfig
defaultPickConflictConfig = PickConflictConfig
  { basicTask = defaultBasicConfig{ atLeastActive = 2 }
  , changeTask = defaultChangeConfig
  }
  
data FindConcurrencyConfig = FindConcurrencyConfig
  { basicTask :: BasicConfig
  , advTask :: AdvConfig
  , changeTask :: ChangeConfig
  } deriving Show
  
defaultFindConcurrencyConfig :: FindConcurrencyConfig
defaultFindConcurrencyConfig = FindConcurrencyConfig
  { basicTask = defaultBasicConfig{ atLeastActive = 3 }
  , advTask = defaultAdvConfig{ presenceOfSourceTransitions = Nothing }
  , changeTask = defaultChangeConfig
  }
  
data PickConcurrencyConfig = PickConcurrencyConfig
  { basicTask :: BasicConfig
  , changeTask :: ChangeConfig
  } deriving Show

defaultPickConcurrencyConfig :: PickConcurrencyConfig
defaultPickConcurrencyConfig = PickConcurrencyConfig
  { basicTask = defaultBasicConfig{ atLeastActive = 2 }
  , changeTask = defaultChangeConfig
  }
  