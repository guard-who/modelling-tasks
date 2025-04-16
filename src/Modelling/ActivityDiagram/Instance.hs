{-# LANGUAGE RecordWildCards #-}
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
import Data.List (singleton)
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
  Action ActionNode
  | Object ObjectNode
  | Decision DecisionNode
  | Merge MergeNode
  | Fork ForkNode
  | Join JoinNode
  | ActivityFinal ActivityFinalNode
  | FlowFinal FlowFinalNode
  | Initial InitialNode
  deriving (Eq, Ord, Show)

data Nodes = Nodes {
  actionNodes  :: Set ActionNode,
  objectNodes  :: Set ObjectNode,
  decisionNodes  :: Set DecisionNode,
  mergeNodes  :: Set MergeNode,
  forkNodes  :: Set ForkNode,
  joinNodes  :: Set JoinNode,
  activityFinalNodes :: Set ActivityFinalNode,
  flowFinalNodes :: Set FlowFinalNode,
  initialNodes :: Set InitialNode
} deriving Show

toSet :: Nodes -> Set Node
toSet ns = S.unions [
  Action `S.mapMonotonic` actionNodes ns,
  Object `S.mapMonotonic` objectNodes ns,
  Decision `S.mapMonotonic` decisionNodes ns,
  Merge `S.mapMonotonic` mergeNodes ns,
  Fork `S.mapMonotonic` forkNodes ns,
  Join `S.mapMonotonic` joinNodes ns,
  ActivityFinal `S.mapMonotonic` activityFinalNodes ns,
  FlowFinal `S.mapMonotonic` flowFinalNodes ns,
  Initial `S.mapMonotonic` initialNodes ns
  ]

parseInstance
  :: MonadThrow m
  => AlloyInstance
  -> m UMLActivityDiagram
parseInstance alloyInstance = do
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
  let nodes' = Nodes {..}
  componentNames <- M.fromAscList . S.toAscList
    <$> getNames scope alloyInstance nodes' "ActionObjectNodes" ComponentName
  let components = enumerateComponents $ toSet nodes'
      names = M.fromList
        $ zip (nubOrd $ M.elems componentNames) $ map singleton ['A'..]
      getName x = fromMaybe "" $ M.lookup x componentNames >>= (`M.lookup` names)
  conns <- getConnections scope alloyInstance nodes'
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
    getAs scope = getX scope alloyInstance


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
  connections = map (uncurry3 AdConnection) $ S.toAscList conns
}

convertToAdNode :: (Node -> String) -> (Node, Int) -> AdNode
convertToAdNode getName tuple = case node of
  Action {} -> AdActionNode {
    label = l,
    name = getName node
    }
  Object {} -> AdObjectNode {
    label = l,
    name = getName node
    }
  Decision {} -> AdDecisionNode {label = l}
  Merge {} -> AdMergeNode {label = l}
  Fork {} -> AdForkNode {label = l}
  Join {} -> AdJoinNode {label = l}
  ActivityFinal {} -> AdActivityFinalNode {label = l}
  FlowFinal {} -> AdFlowFinalNode {label = l}
  Initial {} -> AdInitialNode {label = l}
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
getX scope alloyInstance n f =
  lookupSig (scoped scope n) alloyInstance
  >>= getSingleAs "" (returnX f)

getNames
  :: (MonadThrow m, Ord a)
  => String
  -> AlloyInstance
  -> Nodes
  -> String
  -> (String -> a)
  -> m (Set (Node, a))
getNames scope alloyInstance ns n f = do
  named <- lookupSig (scoped scope n) alloyInstance
  getDoubleAs "name" (toNode ns) (returnX f) named

getConnections
  :: MonadThrow m
  => String
  -> AlloyInstance
  -> Nodes
  -> m (Set (Node, Node, String))
getConnections scope alloyInstance ns = do
  guardNames  <- lookupSig (scoped scope "GuardNames") alloyInstance
  triggers <-  getSingleAs "" (returnX GuardName) guardNames
  activityEdges  <- lookupSig (scoped scope "ActivityEdges") alloyInstance
  from <- getDoubleAs
    "from"
    (returnX Trigger)
    (toNode ns)
    activityEdges
  to <- M.fromAscList . S.toAscList
    <$> getDoubleAs "to" (returnX Trigger) (toNode ns) activityEdges
  label <- M.fromAscList . S.toAscList . only snd triggers
    <$> getDoubleAs
      "guard"
      (returnX Trigger)
      (returnX GuardName)
      activityEdges
  let labelMap :: Map GuardName String
      labelMap = M.fromAscList . zip (S.toAscList triggers) $ map singleton ['a'..]
  return $ link to label labelMap from
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
toNode ns x i = ifX Action ActionNode actionNodes
  $ ifX Object ObjectNode objectNodes
  $ ifX Decision DecisionNode decisionNodes
  $ ifX Merge MergeNode mergeNodes
  $ ifX Fork ForkNode forkNodes
  $ ifX Join JoinNode joinNodes
  $ ifX ActivityFinal ActivityFinalNode activityFinalNodes
  $ ifX FlowFinal FlowFinalNode flowFinalNodes
  $ ifX Initial InitialNode initialNodes
  $ throwM $ NodeHasUnknownNodeType $ "x$" ++ show i
  where
    ifX f g which h =
      let node = toX g x i
      in if node `S.member` which ns
         then return $ f node
         else h
