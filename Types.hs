module Types where 

type Mark = [Int]
type Trans = (Mark,Mark)
data Petri = Petri
  { startM :: Mark
  , trans :: [Trans]
  } deriving Show
  
data Input = Input
  { places :: Int
  , transitions :: Int
  , tkns :: Int
  , maxTkns :: Int
  , maxWght :: Int
  , activated :: Int
  , selfLoops :: Maybe Bool
  , presenceSinkTrans :: Maybe Bool
  , presenceSourceTrans :: Maybe Bool
  , anyOtherFieldThatMightBeNeededLater :: Bool
  }

defaultInput :: Input
defaultInput = Input
  { places = 3
  , transitions = 3
  , tkns = 4
  , maxTkns = 2
  , maxWght = 2
  , activated = 1
  , selfLoops = Nothing
  , presenceSinkTrans = Nothing
  , presenceSourceTrans = Nothing
  , anyOtherFieldThatMightBeNeededLater = undefined -- Note how this field is not even mentioned anywhere below.
  }