module Types where 

import Data.GraphViz.Attributes.Complete (GraphvizCommand (TwoPi, Neato))

type Mark = [Int]
type Trans = (Mark,Mark)
data Petri = Petri
  { startM :: Mark
  , trans :: [Trans]
  } deriving Show
  
data Input = Input
  { places :: Int
  , transitions :: Int
  , atLeastActv :: Int
  , minTknsOv :: Int
  , maxTknsOv :: Int
  , maxTknsPPs :: Int
  , minFlowOv :: Int
  , maxFlowOv :: Int
  , maxFlowPEdge :: Int
  , selfLoops :: Maybe Bool
  , presenceSinkTrans :: Maybe Bool
  , presenceSourceTrans :: Maybe Bool
  , graphLayout :: GraphvizCommand
  , anyOtherFieldThatMightBeNeededLater :: Bool
  }

defaultInput :: Input
defaultInput = Input
  { places = 3
  , transitions = 3
  , atLeastActv = 1
  , minTknsOv = 2
  , maxTknsOv = 4
  , maxTknsPPs = 2
  , minFlowOv = 3
  , maxFlowOv = 6
  , maxFlowPEdge = 2
  , selfLoops = Nothing
  , presenceSinkTrans = Nothing
  , presenceSourceTrans = Nothing
  , graphLayout = TwoPi
  , anyOtherFieldThatMightBeNeededLater = undefined -- Note how this field is not even mentioned anywhere below.
  }