module Types where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi))

type Mark = [Int]
type Trans = (Mark,Mark)
data Petri = Petri
  { startM :: Mark
  , trans :: [Trans]
  } deriving Show
  
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
  , selfLoops :: Maybe Bool
  , presenceSinkTrans :: Maybe Bool
  , presenceSourceTrans :: Maybe Bool
  , graphLayout :: GraphvizCommand
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
  , selfLoops = Nothing
  , presenceSinkTrans = Nothing
  , presenceSourceTrans = Nothing
  , graphLayout = TwoPi
  }