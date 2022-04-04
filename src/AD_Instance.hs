module AD_Instance (
  parseInstance
) where 

import qualified Data.Map               as M (fromAscList, elems, lookup, fromList)
import qualified Data.Set               as S (unions, mapMonotonic, fromAscList, toAscList, map, filter, member)

import AD_Datatype (
  ADConnection(ADConnection),
  ADNode(..),
  UMLActivityDiagram(..),
  )

import Control.Monad.Error.Class        (MonadError (throwError))
import Data.List.Extra                  (nubOrd)
import Data.Map                         (Map)
import Data.Maybe (
  fromMaybe,
  fromJust,
  )
import Data.Set                         (Set)
import Data.String                      (IsString (fromString))
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

newtype TriggerName = TriggerName String
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

newtype ActivityEndNode = ActivityEndNode String
  deriving (Eq, Ord, Read, Show)

newtype FlowEndNode = FlowEndNode String
  deriving (Eq, Ord, Read, Show)

newtype StartNode = StartNode String
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
  | AeNode ActivityEndNode
  | FeNode FlowEndNode
  | StNode StartNode
  deriving (Eq, Ord, Show)

data Nodes = Nodes {
  aNodes  :: Set ActionNode,
  oNodes  :: Set ObjectNode,
  dNodes  :: Set DecisionNode,
  mNodes  :: Set MergeNode,
  fNodes  :: Set ForkNode,
  jNodes  :: Set JoinNode,
  aeNodes :: Set ActivityEndNode,
  feNodes :: Set FlowEndNode,
  stNodes :: Set StartNode
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

 --To be finished later
parseInstance :: (MonadError s m, IsString s)
  => String
  -> AlloyInstance
  -> m UMLActivityDiagram
parseInstance scope insta = do  
  actionNodes <- getAs "ActionNodes" ActionNode
  objectNodes <- getAs "ObjectNodes" ObjectNode
  decisionNodes <- getAs "DecisionNodes" DecisionNode
  mergeNodes <- getAs "MergeNodes" MergeNode
  forkNodes <- getAs "ForkNodes" ForkNode
  joinNodes <- getAs "JoinNodes" JoinNode
  activityEndNodes <- getAs "ActivityEndNodes" ActivityEndNode
  flowEndNodes <- getAs "FlowEndNodes" FlowEndNode
  startNodes <- getAs "StartNodes" StartNode
  let nodes' = Nodes actionNodes objectNodes decisionNodes mergeNodes forkNodes joinNodes activityEndNodes flowEndNodes startNodes
  cnames <- fmap (M.fromAscList . S.toAscList) $ getNames scope insta nodes' "States" ComponentName
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
      :: (IsString s, MonadError s m, Ord a)
      => String
      -> (String -> a)
      -> m (Set a)
    getAs = getX scope insta


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
  nodes = map (convertToADNode getName) (S.toAscList components),
  connections = uncurry3 ADConnection <$> S.toAscList conns
}
    
convertToADNode :: (Node -> String) -> (Node, Int) -> ADNode 
convertToADNode getName tuple = case node of 
  ANode {} -> ADActionNode {
    label = l,
    name = getName node
    }
  ONode {} -> ADObjectNode {
    label = l,
    name = getName node
    }
  DNode {} -> ADDecisionNode { label = l }
  MNode {} -> ADMergeNode { label = l }
  FNode {} -> ADForkNode { label = l }
  JNode {} -> ADJoinNode { label = l }
  AeNode {} -> ADActivityEndNode { label = l }
  FeNode {} -> ADFlowEndNode { label = l }
  StNode {} -> ADStartNode { label = l }
  where 
    node = fst tuple
    l = snd tuple


toX :: (String -> a) -> String -> Int -> a
toX f x = f . (x ++) . ('$':) . show

returnX :: Monad m => (String -> a) -> String -> Int -> m a
returnX x y = return . toX x y

getX
  :: (MonadError s m, IsString s, Ord a)
  => String
  -> AlloyInstance
  -> String
  -> (String -> a)
  -> m (Set a)
getX scope insta n f =
  lookupSig (scoped scope n) insta
  >>= getSingleAs "" (returnX f)

getNames
  :: (MonadError s m, IsString s, Ord a)
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
  :: (MonadError s m, IsString s)
  => String
  -> AlloyInstance
  -> Nodes
  -> m (Set (Node, Node, String))
getConnections scope insta ns = do
  triggerNames  <- lookupSig (scoped scope "TriggerNames") insta
  triggers <-  getSingleAs "" (returnX TriggerName) triggerNames
  realFlows  <- lookupSig (scoped scope "Flows") insta
  protoFlows <- lookupSig (scoped scope "ProtoFlows") insta
  flows <- getSingleAs "" (returnX Trigger) realFlows
  from <- only fst flows <$> getDoubleAs
    "from"
    (returnX Trigger)
    (toNode ns)
    protoFlows
  to <- M.fromAscList . S.toAscList . only fst flows
    <$> getDoubleAs "to" (returnX Trigger) (toNode ns) protoFlows
  tlabel <- M.fromAscList . S.toAscList . only snd triggers .  only fst flows
    <$> getDoubleAs
      "label"
      (returnX Trigger)
      (returnX TriggerName)
      realFlows
  let labelMap :: Map TriggerName String
      labelMap = M.fromAscList . zip (S.toAscList triggers) $ pure <$> ['a'..]
  return $ link to tlabel labelMap from
  where
    only f xs = S.filter $ (`S.member` xs) . f
    link to lbs lm = S.map $ \(x, f) -> (
      f,
      fromJust $ M.lookup x to,
      fromMaybe "" $ M.lookup x lbs >>= (`M.lookup` lm)
      )

toNode
  :: (MonadError s m, IsString s)
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
  $ ifX AeNode ActivityEndNode aeNodes
  $ ifX FeNode FlowEndNode feNodes
  $ ifX StNode StartNode stNodes
  $ throwError $ fromString $ "unknown node x$" ++ show i
  where
    ifX f g which h =
      let node = toX g x i
      in if node `S.member` which ns
         then return $ f node
         else h