module Modelling.PetriNet.Types where

import Data.GraphViz.Attributes.Complete (GraphvizCommand (Circo))

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
  , graphLayout = Circo
  }
  
data PetriAdvConfig = PetriAdvConfig
  { presenceOfSelfLoops :: Maybe Bool
  , presenceOfSinkTransitions :: Maybe Bool
  , presenceOfSourceTransitions :: Maybe Bool
  }
  
defaultPetriAdvConfig :: PetriAdvConfig
defaultPetriAdvConfig = PetriAdvConfig
  { presenceOfSelfLoops = Just False
  , presenceOfSinkTransitions = Just False
  , presenceOfSourceTransitions = Just False
  }

data PetriChangeConfig = PetriChangeConfig
  { tokenChangeOverall :: Int
  , maxTokenChangePerPlace :: Int
  , flowChangeOverall :: Int
  , maxFlowChangePerEdge :: Int
  }
  
defaultPetriChangeConfig :: PetriChangeConfig
defaultPetriChangeConfig = PetriChangeConfig
  { tokenChangeOverall = 0
  , maxTokenChangePerPlace = 0
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }

data PetriTask1Config = PetriTask1Config 
  { basicTask1 :: PetriBasicConfig
  , advTask1 :: PetriAdvConfig
  , changeTask1 :: PetriChangeConfig
  }
  
defaultPetriTask1Config :: PetriTask1Config
defaultPetriTask1Config = PetriTask1Config
  { basicTask1 = defaultPetriBasicConfig
  , advTask1 = defaultPetriAdvConfig
  , changeTask1 = defaultPetriChangeConfig
  }
  