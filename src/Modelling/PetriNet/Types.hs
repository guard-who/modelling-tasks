{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Types where

import Data.GraphViz.Attributes.Complete (GraphvizCommand (Circo))

data Change = Change {tokenChange :: [(String,Int)],flowChange :: [(String,String,Int)]}
  deriving (Eq,Show)
  
data Conflict = Conflict{conflictTrans :: (String,String),conflictPlace :: String}
  deriving Show
  
type Concurrent = [String]

type Marking = [Int]
type Transition = (Marking,Marking)
data Petri = Petri
  { initialMarking :: Marking
  , trans :: [Transition]
  } deriving (Eq,Show)
  
defaultPetri :: Petri
defaultPetri = Petri
  { initialMarking = [1,1,0]
  , trans = [([1,0,0],[0,1,0]),([1,0,0],[0,0,1]),([0,1,1],[2,0,0])]
  }
  
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
  }

defaultBasicConfig :: BasicConfig
defaultBasicConfig = BasicConfig
  { places = 3
  , transitions = 3
  , atLeastActive = 1
  , minTokensOverall = 2
  , maxTokensOverall = 4
  , maxTokensPerPlace = 2
  , minFlowOverall = 3
  , maxFlowOverall = 6
  , maxFlowPerEdge = 2
  , graphLayout = Circo
  }
  
data AdvConfig = AdvConfig
  { presenceOfSelfLoops :: Maybe Bool
  , presenceOfSinkTransitions :: Maybe Bool
  , presenceOfSourceTransitions :: Maybe Bool
  }
  
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
  }
  
defaultChangeConfig :: ChangeConfig
defaultChangeConfig = ChangeConfig
  { tokenChangeOverall = 0
  , maxTokenChangePerPlace = 0
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }

data MathConfig = MathConfig 
  { basicTask :: BasicConfig
  , advTask :: AdvConfig
  , changeTask :: ChangeConfig
  }
  
defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig
  { basicTask = defaultBasicConfig
  , advTask = defaultAdvConfig
  , changeTask = defaultChangeConfig
  }
  
data ConflictConfig = ConflictConfig
  { basicTask :: BasicConfig
  }

defaultConflictConfig :: ConflictConfig
defaultConflictConfig = ConflictConfig
  { basicTask = defaultBasicConfig
  }
  