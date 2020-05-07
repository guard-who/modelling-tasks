module Types where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))

--tokenChange,flowChange
data Change = Change {tknChange :: [(String,Int)],flwChange :: [(String,String,Int)]}
  deriving Show

type Mark = [Int]
type Trans = (Mark,Mark)
data Petri = Petri
  { startM :: Mark
  , trans :: [Trans]
  } deriving (Eq,Show)
  
defaultPetri :: Petri
defaultPetri = Petri
  { startM = [1,1,0]
  , trans = [([1,0,0],[0,1,0]),([1,0,0],[0,0,1]),([0,1,1],[2,0,0])]
  }
  
data Input = Input
  { places :: Int
  , transitions :: Int
  , atLeastActiv :: Int
  , minTknsOverall :: Int
  , maxTknsOverall :: Int
  , maxTknsPerPlace :: Int
  , minFlowOverall :: Int
  , maxFlowOverall :: Int
  , maxFlowPerEdge :: Int
  , presenceSelfLoops :: Maybe Bool
  , presenceSinkTrans :: Maybe Bool
  , presenceSourceTrans :: Maybe Bool
  , graphLayout :: GraphvizCommand
  , tokenChangeOverall :: Int
  , maxTokenChangePerPlace :: Int
  , flowChangeOverall :: Int
  , maxFlowChangePerEdge :: Int
  }

defaultInput :: Input
defaultInput = Input
  { places = 3
  , transitions = 3
  , atLeastActiv = 1
  , minTknsOverall = 2
  , maxTknsOverall = 4
  , maxTknsPerPlace = 2
  , minFlowOverall = 3
  , maxFlowOverall = 6
  , maxFlowPerEdge = 2
  , presenceSelfLoops = Nothing
  , presenceSinkTrans = Nothing
  , presenceSourceTrans = Nothing
  , graphLayout = TwoPi
  , tokenChangeOverall = 0
  , maxTokenChangePerPlace = 0
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }