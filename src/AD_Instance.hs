module AD_Instance (
  parseInstance
) where 

-- import Data.Set               as S (mapMonotonic, unions)

import AD_Datatype (
  UMLActivityDiagram(UMLActivityDiagram),
  )

import Control.Monad.Error.Class        (MonadError (throwError))

import Data.Set                         (Set)
import Data.String                      (IsString (fromString))


import Language.Alloy.Call (
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

newtype EndNode = EndNode String
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
  | ENode EndNode
  | StNode StartNode
  deriving (Eq, Ord, Show)

data Nodes = Nodes {
  aNodes  :: Set ActionNode,
  oNodes  :: Set ObjectNode,
  dNodes  :: Set DecisionNode,
  mNodes  :: Set MergeNode,
  fNodes  :: Set ForkNode,
  jNodes  :: Set JoinNode,
  eNodes  :: Set EndNode,
  stNodes :: Set StartNode
} deriving Show

{- Not used yet
toSet :: Nodes -> Set Node
toSet ns = S.unions [
  ANode `S.mapMonotonic` aNodes ns,
  ONode `S.mapMonotonic` oNodes ns,
  DNode `S.mapMonotonic` dNodes ns,
  MNode `S.mapMonotonic` mNodes ns,
  FNode `S.mapMonotonic` fNodes ns,
  JNode `S.mapMonotonic` jNodes ns,
  ENode `S.mapMonotonic` eNodes ns,
  StNode `S.mapMonotonic` stNodes ns
  ]
 -}

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
  endNodes <- getAs "EndNodes" EndNode
  startNodes <- getAs "StartNodes" StartNode
  let nodes = Nodes actionNodes objectNodes decisionNodes mergeNodes forkNodes joinNodes endNodes startNodes
  return (UMLActivityDiagram [] [] [])
  where
    getAs
      :: (IsString s, MonadError s m, Ord a)
      => String
      -> (String -> a)
      -> m (Set a)
    getAs = getX scope insta



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