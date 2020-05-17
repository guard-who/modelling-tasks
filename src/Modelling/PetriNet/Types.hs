module Modelling.PetriNet.Types where

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))

data Change = Change {tokenChange :: [(String,Int)],flowChange :: [(String,String,Int)]}
  deriving (Eq,Show)
  
data Conflict = Conflict{conflictTrans :: (String,String),conflictPlace :: String}
  deriving Show

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
  
data PetriBasicConfig = PetriBasicConfig
  { places :: Int
  , transitions :: Int
  , atLeastActive :: Int
  , minTokensOverall :: Int
  , maxTokensOverall :: Int
  , maxTokensPerPlace :: Int
  , minFlowOverall :: Int
  , maxFlowOverall :: Int
  , maxFlowPerEdge :: Int
  , presenceOfSelfLoops :: Maybe Bool
  , presenceOfSinkTransitions :: Maybe Bool
  , presenceOfSourceTransitions :: Maybe Bool
  , graphLayout :: GraphvizCommand
  }

defaultPetriBasicConfig :: PetriBasicConfig
defaultPetriBasicConfig = PetriBasicConfig
  { places = 3
  , transitions = 3
  , atLeastActive = 1
  , minTokensOverall = 2
  , maxTokensOverall = 4
  , maxTokensPerPlace = 2
  , minFlowOverall = 3
  , maxFlowOverall = 6
  , maxFlowPerEdge = 2
  , presenceOfSelfLoops = Just False
  , presenceOfSinkTransitions = Just False
  , presenceOfSourceTransitions = Just False
  , graphLayout = TwoPi
  }

data PetriTask1Config = PetriTask1Config 
  { basicTask1 :: PetriBasicConfig
  , tokenChangeOverall :: Int
  , maxTokenChangePerPlace :: Int
  , flowChangeOverall :: Int
  , maxFlowChangePerEdge :: Int
  }
  
defaultPetriTask1Config :: PetriTask1Config
defaultPetriTask1Config = PetriTask1Config
  { basicTask1 = defaultPetriBasicConfig
  , tokenChangeOverall = 0
  , maxTokenChangePerPlace = 0
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }
  