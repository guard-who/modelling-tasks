{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Types where

import Data.GraphViz.Attributes.Complete (GraphvizCommand (Neato))

data Change = Change {tokenChange :: [(String,Int)],flowChange :: [(String,String,Int)]}
  deriving (Eq,Show)
  
data Conflict = Conflict{conflictTrans :: (String,String),conflictPlace :: String}
  deriving Show
  
type Concurrent = (String,String)

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
  , advTask = defaultAdvConfig{ presenceOfSinkTransitions = Nothing, presenceOfSourceTransitions = Nothing }
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
  , advTask = defaultAdvConfig{ presenceOfSinkTransitions = Nothing, presenceOfSourceTransitions = Nothing }
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
  