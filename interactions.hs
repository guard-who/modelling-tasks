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
#{modulePetriSignature}
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
modulePetriSignature :: String
modulePetriSignature = [i|
abstract sig Nodes
{
  flow : Nodes -> lone Int,
  defaultFlow : Nodes -> lone Int,
  flowChange : Nodes -> lone (Int - 0)
}
{
  all weight : defaultFlow[Nodes] + flow[Nodes] | weight > 0
  all n : Nodes | let theFlow = plus[defaultFlow[n], flowChange[n]] | theFlow = 0 implies no flow[n] else flow[n] = theFlow
}

abstract sig Places extends Nodes
{
  defaultTokens : one Int,
  tokenChange : lone (Int - 0),
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
  tokens >= 0
  //set place only going to transition
  flow.Int in Transitions
  defaultFlow.Int in Transitions
}

abstract sig Transitions extends Nodes
{
}
{
  //set transition only going to place
  flow.Int in Places
  defaultFlow.Int in Places
}

//set default places and transitions
abstract sig givenPlaces extends Places{}
abstract sig givenTransitions extends Transitions{}
|]
---------------------------------------------
modulePetriConstraints :: String
modulePetriConstraints = [i|
//#{modulePetriConcepts}

//set tokens should be added to a petri net only
pred tokenAddOnly[]{
  all tc : Places.tokenChange | tc > 0
}

//set tokens should be removed from a petri net only
pred tokenRemoveOnly[]{
  all tc : Places.tokenChange | tc < 0
}

//check if maximum set of concurrent transitions
pred isMaxConcurrency[ts : set Transitions]{
  concurrent[ts]
  no t : (Transitions - ts) | concurrent[ts+t]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  tokenChangeSum[Places] = n
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  tokenChangeSum[Places] = minus[0,n]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Places | p.tokenChange =< m
}

//set weight can be added to a petri net only
pred weightAddOnly[]{
  all change : Nodes.flowChange[Nodes] | change > 0
}

//set weight can be removed from a petri net only
pred weightRemoveOnly[]{
  all change : Nodes.flowChange[Nodes] | change < 0
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  flowChangeSum[Nodes,Nodes] = n
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  flowChangeSum[Nodes,Nodes] = minus[0,n]
}

//altogether exactly n transitions should be activated
pred numberActivatedTransition[n : Int, ts : set Transitions]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transitions - ts) | activated[t]
}
|]
---------------------------------------------
modulePetriAdditions :: String
modulePetriAdditions = [i|

//#{modulePetriSignature}

//Places and Transitions to be added
sig addedPlaces extends Places{}
{
  defaultTokens = 0
  no defaultFlow
}

sig addedTransitions extends Transitions{}
{
  no defaultFlow
}


pred noChangesToGivenParts[]{
  no givenPlaces.tokenChange
  let givenNodes = givenPlaces + givenTransitions | no givenNodes.flowChange[givenNodes]
}
|]
---------------------------------------------
modulePetriConcepts :: String
modulePetriConcepts = [i|

//#{modulePetriSignature}
//#{modulePetriHelpers}

//check if a transition is activated
pred activated[t : Transitions]{
  all p : Places | p.tokens >= p.flow[t]
}

//check if a transition conflicts with another transitions
pred conflict[t1, t2 : Transitions, p : Places]{
  t1 != t2
  activated[t1]
  activated[t2]
  p.tokens < plus[p.flow[t1], p.flow[t2]]
}

//check if two distinct transitions are concurrent
pred concurrent[ts : set Transitions]{
  all p : Places | p.tokens >= flowSum[p, ts]
}

//check activation under default condition
 pred activatedDefault[t : Transitions]{
  all p : Places | p.defaultTokens >= p.defaultFlow[t]
}

//check conflict under default condition
pred conflictDefault[t1, t2 : Transitions, p : Places]{
  t1 != t2
  activatedDefault[t1]
  activatedDefault[t2]
  p.defaultTokens < plus[p.defaultFlow[t1], p.defaultFlow[t2]]
}

//check concurrent under default condition
pred concurrentDefault[ts : set Transitions]{
  all p : Places | p.defaultTokens >= defaultFlowSum[p, ts]
}

//check if there is a loop between a place and a transition
pred selfLoop[p : Places, t : Transitions]{
  (one p.flow[t]) and (one t.flow[p])
}

//check if some transitions are sink transitions
pred sinkTransitions[ts : set Transitions]{
  no ts.flow
}

//check if some transitions are source transitions
pred sourceTransitions[ts : set Transitions]{
  no Places.flow[ts]
}
|]
---------------------------------------------
modulePetriHelpers :: String
modulePetriHelpers = [i|

//#{modulePetriSignature}

//sum of tokenChange
fun tokenChangeSum[places : set Places] : Int{
  sum p : places | p.tokenChange
}

//total number of flows going from set of nodes to set of nodes
fun flowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flow[t]
}

//total number of default flows going out from set of nodes to a set of nodes
fun defaultFlowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.defaultFlow[t]
}

//total number of flow changes going out from set of nodes to a set of nodes
fun flowChangeSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flowChange[t]
}
|]

