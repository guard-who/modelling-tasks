module Modelling.ActivityDiagram.Instance (
  parseInstance
) where

import qualified Data.Map               as M (fromAscList, elems, lookup, fromList)
import qualified Data.Set               as S (unions, mapMonotonic, fromAscList, toAscList, map, filter, member)

import Modelling.ActivityDiagram.Datatype (
  AdConnection (AdConnection),
  AdNode (..),
  UMLActivityDiagram(..),
  )

import Control.Monad.Catch              (Exception, MonadThrow (throwM))
import Data.List.Extra                  (nubOrd)
import Data.Map                         (Map)
import Data.Maybe (
  fromMaybe,
  fromJust,
  )
import Data.Set                         (Set)
import Data.Tuple.Extra                 (uncurry3)
import Language.Alloy.Call (
  getDoubleAs,
  getSingleAs,
  lookupSig,
  scoped,
  AlloyInstance,
  )

newtype ComponentName = ComponentName String
  deriving (Eq, Ord, Read, Show)

newtype GuardName = GuardName String
  deriving (Eq, Ord, Read, Show)

newtype ActionNode = ActionNode String
  deriving (Eq, Ord, Read, Show)

newtype ObjectNode = ObjectNode String
  deriving (Eq, Ord, Read, Show)

newtype DecisionNode = DecisionNode String
  deriving (Eq, Ord, Read, Show)

newtype MergeNode = MergeNode String
  deriving (Eq, Ord, Read, Show)

newtype ForkNode = ForkNode String
  deriving (Eq, Ord, Read, Show)

newtype JoinNode = JoinNode String
  deriving (Eq, Ord, Read, Show)

newtype ActivityFinalNode = ActivityFinalNode String
  deriving (Eq, Ord, Read, Show)

newtype FlowFinalNode = FlowFinalNode String
  deriving (Eq, Ord, Read, Show)

newtype InitialNode = InitialNode String
  deriving (Eq, Ord, Read, Show)

newtype Trigger = Trigger String
  deriving (Eq, Ord, Read, Show)

data Node =
  ANode ActionNode
  | ONode ObjectNode
  | DNode DecisionNode
  | MNode MergeNode
  | FNode ForkNode
  | JNode JoinNode
  | AeNode ActivityFinalNode
  | FeNode FlowFinalNode
  | StNode InitialNode
  deriving (Eq, Ord, Show)

data Nodes = Nodes {
  aNodes  :: Set ActionNode,
  oNodes  :: Set ObjectNode,
  dNodes  :: Set DecisionNode,
  mNodes  :: Set MergeNode,
  fNodes  :: Set ForkNode,
  jNodes  :: Set JoinNode,
  aeNodes :: Set ActivityFinalNode,
  feNodes :: Set FlowFinalNode,
  stNodes :: Set InitialNode
} deriving Show

toSet :: Nodes -> Set Node
toSet ns = S.unions [
  ANode `S.mapMonotonic` aNodes ns,
  ONode `S.mapMonotonic` oNodes ns,
  DNode `S.mapMonotonic` dNodes ns,
  MNode `S.mapMonotonic` mNodes ns,
  FNode `S.mapMonotonic` fNodes ns,
  JNode `S.mapMonotonic` jNodes ns,
  AeNode `S.mapMonotonic` aeNodes ns,
  FeNode `S.mapMonotonic` feNodes ns,
  StNode `S.mapMonotonic` stNodes ns
  ]

parseInstance
  :: MonadThrow m
  => AlloyInstance
  -> m UMLActivityDiagram
parseInstance insta = do
  let scope = "this"
  actionNodes <- getAs scope "ActionNodes" ActionNode
  objectNodes <- getAs scope"ObjectNodes" ObjectNode
  decisionNodes <- getAs scope "DecisionNodes" DecisionNode
  mergeNodes <- getAs scope "MergeNodes" MergeNode
  forkNodes <- getAs scope "ForkNodes" ForkNode
  joinNodes <- getAs scope "JoinNodes" JoinNode
  activityFinalNodes <- getAs scope "ActivityFinalNodes" ActivityFinalNode
  flowFinalNodes <- getAs scope"FlowFinalNodes" FlowFinalNode
  initialNodes <- getAs scope "InitialNodes" InitialNode
  let nodes' = Nodes actionNodes objectNodes decisionNodes mergeNodes forkNodes joinNodes activityFinalNodes flowFinalNodes initialNodes
  cnames <- M.fromAscList . S.toAscList <$> getNames scope insta nodes' "ActionObjectNodes" ComponentName
  let components = enumerateComponents $ toSet nodes'
      names = M.fromList $ zip (nubOrd $ M.elems cnames) $ pure <$> ['A'..]
      getName x = fromMaybe "" $ M.lookup x cnames >>= (`M.lookup` names)
  conns <- getConnections scope insta nodes'
  let labelOf = getLabelOf components
      conns' = S.map (\(x, y, z) -> (labelOf x, labelOf y, z)) conns
      activityDiagram = setToActivityDiagram getName components conns'
  return activityDiagram
  where
    getAs
      :: (MonadThrow m, Ord a)
      => String
      -> String
      -> (String -> a)
      -> m (Set a)
    getAs scope = getX scope insta


enumerateComponents :: Set Node -> Set (Node, Int)
enumerateComponents s = S.fromAscList $ zip (S.toAscList s) [1..]

getLabelOf :: Set (Node, Int) -> Node -> Int
getLabelOf s n = fromJust $ M.lookup n (M.fromAscList $ S.toAscList s)

setToActivityDiagram
  :: (Node -> String)
  -> Set (Node, Int)
  -> Set (Int, Int, String)
  -> UMLActivityDiagram
setToActivityDiagram getName components conns = UMLActivityDiagram {
  nodes = map (convertToAdNode getName) (S.toAscList components),
  connections = uncurry3 AdConnection <$> S.toAscList conns
}

convertToAdNode :: (Node -> String) -> (Node, Int) -> AdNode
convertToAdNode getName tuple = case node of
  ANode {} -> AdActionNode {
    label = l,
    name = getName node
    }
  ONode {} -> AdObjectNode {
    label = l,
    name = getName node
    }
  DNode {} -> AdDecisionNode {label = l}
  MNode {} -> AdMergeNode {label = l}
  FNode {} -> AdForkNode {label = l}
  JNode {} -> AdJoinNode {label = l}
  AeNode {} -> AdActivityFinalNode {label = l}
  FeNode {} -> AdFlowFinalNode {label = l}
  StNode {} -> AdInitialNode {label = l}
  where
    node = fst tuple
    l = snd tuple


toX :: (String -> a) -> String -> Int -> a
toX f x = f . (x ++) . ('$':) . show

returnX :: Monad m => (String -> a) -> String -> Int -> m a
returnX x y = return . toX x y

getX
  :: (MonadThrow m, Ord a)
  => String
  -> AlloyInstance
  -> String
  -> (String -> a)
  -> m (Set a)
getX scope insta n f =
  lookupSig (scoped scope n) insta
  >>= getSingleAs "" (returnX f)

getNames
  :: (MonadThrow m, Ord a)
  => String
  -> AlloyInstance
  -> Nodes
  -> String
  -> (String -> a)
  -> m (Set (Node, a))
getNames scope insta ns n f = do
  named <- lookupSig (scoped scope n) insta
  getDoubleAs "name" (toNode ns) (returnX f) named

getConnections
  :: MonadThrow m
  => String
  -> AlloyInstance
  -> Nodes
  -> m (Set (Node, Node, String))
getConnections scope insta ns = do
  guardNames  <- lookupSig (scoped scope "GuardNames") insta
  triggers <-  getSingleAs "" (returnX GuardName) guardNames
  activityEdges  <- lookupSig (scoped scope "ActivityEdges") insta
  from <- getDoubleAs
    "from"
    (returnX Trigger)
    (toNode ns)
    activityEdges
  to <- M.fromAscList . S.toAscList
    <$> getDoubleAs "to" (returnX Trigger) (toNode ns) activityEdges
  tlabel <- M.fromAscList . S.toAscList . only snd triggers
    <$> getDoubleAs
      "guard"
      (returnX Trigger)
      (returnX GuardName)
      activityEdges
  let labelMap :: Map GuardName String
      labelMap = M.fromAscList . zip (S.toAscList triggers) $ pure <$> ['a'..]
  return $ link to tlabel labelMap from
  where
    only f xs = S.filter $ (`S.member` xs) . f
    link to lbs lm = S.map $ \(x, f) -> (
      f,
      fromJust $ M.lookup x to,
      fromMaybe "" $ M.lookup x lbs >>= (`M.lookup` lm)
      )

newtype ParsingActivityDiagramException
  = NodeHasUnknownNodeType String
  deriving Show

instance Exception ParsingActivityDiagramException

toNode
  :: MonadThrow m
  => Nodes
  -> String
  -> Int
  -> m Node
toNode ns x i = ifX ANode ActionNode aNodes
  $ ifX ONode ObjectNode oNodes
  $ ifX DNode DecisionNode dNodes
  $ ifX MNode MergeNode mNodes
  $ ifX FNode ForkNode fNodes
  $ ifX JNode JoinNode jNodes
  $ ifX AeNode ActivityFinalNode aeNodes
  $ ifX FeNode FlowFinalNode feNodes
  $ ifX StNode InitialNode stNodes
  $ throwM $ NodeHasUnknownNodeType $ "x$" ++ show i
  where
    ifX f g which h =
      let node = toX g x i
      in if node `S.member` which ns
         then return $ f node
         else h
