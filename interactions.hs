{-# LANGUAGE ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}

module Interactions where 

import Data.String.Interpolate
import Text.Read

data Preferences = Preferences { places :: Int ,
  trans :: Int ,
  deadL :: Maybe Bool ,
  lifely:: Maybe Bool
  } deriving (Show)
  
--expliziter Datentyp? (Stellen, Trans, Maybe BoolDeadL, Maybe BoolLebend ,...)

--Inputs in List
initInput :: IO ()
initInput = do
  putStrLn "Geben Sie nacheinander an: \n - Anzahl Stellen \n - Anzahl Transitionen"
  putStrLn " - Folgende Zahlen als Zusatz: 0: DeadLock-Frei | 1: mit DeadLock | 2: Lebendig | exit: Ende"
  val <- getInputs
  putStrLn (show val)

getInputs :: IO [Int]
getInputs = do 
  putStr "Eingabe: "
  val <- getLine
  case parseInput val of 
    Nothing -> return []
    Just aNum -> do
      nextInputs <- getInputs
      return (aNum : nextInputs)
  
parseInput :: String -> Maybe Int
parseInput input = if input == "exit" then Nothing else (readMaybe input):: Maybe Int


--input in expl. Datentyp
getInput :: IO Preferences
getInput = do
  putStrLn "Angaben mit * können übersprungen werden (Eingabe)"
  putStr "Anzahl der Stellen: "
  pls <- getLine
  putStr "Anzahl der Transitionen: "
  trns <- getLine
  putStr "* Mit DeadLock?(True/False)"
  dl <- getLine
  putStr "* Lebendig? (True/False)"
  ll <- getLine
  let plsI = (read pls :: Int)
  let trnsI = (read trns :: Int)
  let dlB = transYN dl
  let llB = transYN ll
  return Preferences{ places=plsI , trans=trnsI , deadL = dlB , lifely = llB}
  
printInput :: IO ()
printInput = do
  val <- getInput 
  print val
  
transYN :: String -> Maybe Bool
transYN input = if input == "" then Nothing else (readMaybe input):: Maybe Bool

--Umwandlung: Input -> AlloyData
testIO :: IO ()
testIO = do 
  pref <- getInput
  putStr $ petriAlloy pref

--Merging
petriAlloy :: Preferences -> String 
petriAlloy pref = [i|
module scenarios/examples/PetriNetSample
//default Petri net

#{petriPlaces (places pref)}
#{petriTrans (trans pref)}

#{petriFlow (places pref) (trans pref) [1,1,0]}
|]

--Stellen
petriPlaces :: Int -> String
petriPlaces 0 = "\n"
petriPlaces i = "one sig S" ++ show i ++" extends givenPlaces{} \n"++ petriPlaces (i-1)

--Transitionen
petriTrans :: Int -> String
petriTrans 0 = "\n"
petriTrans i = "one sig T" ++ show i ++" extends givenTransitions{} \n"++ petriTrans (i-1)

--defaultFlow
petriFlow :: Int -> Int -> [Int] -> String
petriFlow p t list = "fact { \n" ++
  (petriTokens 1 p list) ++ (petriDFlowP p) ++ (petriDFlowT t)
  ++ "} \n"
  
--help -> right order, always start with 1| Startmarkierung
petriTokens ::Int -> Int -> [Int] -> String
petriTokens _ 0 [] = "\n"
petriTokens _ _ [] = "error"
petriTokens help p (h:tail) = "  S"++ show help ++".defaultTokens = " ++ show h ++"\n"
  ++ petriTokens (help+1) (p-1) tail
  
                                   --Überarbeiten--
--Ohne default Flow, kommen keine Verbindungen zustande!
petriDFlowP :: Int -> String 
petriDFlowP 0 = "\n"
petriDFlowP p = "  no S"++ show p ++".defaultFlow[Transitions] \n"++ petriDFlowP (p-1)

petriDFlowT :: Int -> String
petriDFlowT 0 = "\n"
petriDFlowT t = "  no T"++ show t ++".defaultFlow[Places] \n"++ petriDFlowT (t-1)

--------------------------------------------------------
