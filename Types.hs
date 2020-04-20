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
  , petriScope :: Int
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
  , petriScope = 10
  , anyOtherFieldThatMightBeNeededLater = undefined -- Note how this field is not even mentioned anywhere below.
  }