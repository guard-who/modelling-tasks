{-# Language QuasiQuotes #-}
module Functions where 

import Data.List

type Mark = [Int]
type Trans = (Mark,Mark)
type Petri = (Mark,[Trans])
type TGNode = (Mark,[Mark])
data FormalNet = PetriNet{mark :: Int,
  trans :: Int ,
  preT :: [(Int,Mark)],
  postT :: [(Int,Mark)],
  startM :: Int
  }

-- (Stellen, Transitionen, Vorbedingungen, Nachbedingungen, Startmarkierung)
-- FormalNet datatype einfügen als rückgabe
getFormal :: Mark -> [Trans] -> ( Int , Int , [(Int,Mark)] , [(Int,Mark)] , Mark)
getFormal _ [] = error "Lost Trans Error"
getFormal sm tr = ( getMarks tr , length tr , getPre 1 tr , getPost 1 tr , sm)

getMarks :: [Trans] -> Int
getMarks [] = 0
getMarks ((vb,nb):rs) = length vb

getPre ::Int -> [Trans] -> [(Int,Mark)]
getPre _ [] = []
getPre i ((pre,_):rs) = (i,pre):(getPre (i+1) rs)

getPost ::Int -> [Trans] -> [(Int,Mark)]
getPost _ [] = []
getPost i ((_,post):rs) = (i,post):(getPost (i+1) rs)

--Make an output from the Data
printFormal :: Mark -> [Trans] -> IO ()
printFormal sm tr = do 
  let (m, t, pre, post, stm) = getFormal sm tr
  print ("S={" ++reverse (formMarks m)++ "} " ++
         "T={" ++reverse (formTrans t)++ "} "
        )
  print $ formPre pre
  print $ formPost post
  print $ "m0=(" ++ convertList stm ++ ")"
  
formMarks :: Int -> String
formMarks 0 = ""
formMarks 1 = "1s"
formMarks m = show m++ "s, " ++ formMarks (m-1)

formTrans :: Int -> String
formTrans 0 = ""
formTrans 1 = "1t"
formTrans t = show t++ "t, " ++ formTrans (t-1)

formPre :: [(Int,Mark)] -> String
formPre [] = ""
formPre ((i,m):rs) = ".t" ++ show i ++"=("++ convertList m ++") " ++ formPre rs

formPost :: [(Int,Mark)] -> String
formPost [] = ""
formPost ((i,m):rs) = "t." ++ show i ++"=("++ convertList m ++") " ++ formPost rs

convertList :: [Int] -> String
convertList [] = ""
convertList (a:[]) = show a
convertList (a:rs) = show a ++"," ++ convertList rs

--convert to TransitionGraph
--filled in function -> startM -> TransP -> return
getTransitionGraph :: [Mark] -> [Mark] -> [Trans] -> [TGNode]
getTransitionGraph _ [] _ = []
getTransitionGraph um (m:rm) t = ((m,calcM m t) : getTransitionGraph (m:um) (union rm (cutList (m:um) (calcM m t))) t)

--Getting all Links for a Mark in TG
calcM :: Mark -> [Trans] -> [Mark]
calcM _ [] = []
calcM m ((pr,po):rs) 
  | checkMark  m pr == True = ((zipWith (+) (zipWith (-) m pr) po):calcM m rs)
  | otherwise               = calcM m rs

checkMark :: Mark -> Mark -> Bool 
checkMark [] _ = True
checkMark (m:rm) (pr:rpr)
  | m >= pr = checkMark rm rpr
  | otherwise = False

cutList :: [Mark] -> [Mark] -> [Mark]
cutList _ [] = []
cutList l1 (h:rs)
  | elem h l1 == False = (h:cutList l1 rs)
  | otherwise          =  cutList l1 rs
  
--generate Tasks
--Tasks generated with Options
--each Option gets a value with a boolean attached for right and false answer
--new File later

type TOpt a = [([a],Bool)]
data TGHelp = TGHelp [Mark] [Trans]

--Task: Petri -> TG
--generate Transitionsgraphs
genTGTask :: Int -> Petri -> TOpt TGNode
genTGTask i (m,tr) = ( (getTransitionGraph [] [m] tr, True) : genTGOpt i (m,tr) )

-- Petrinet change & gen false Opt
--                       -> [([TGNode],Bool)]
-- case für auswahl von veränderung?
genTGOpt :: Int -> Petri -> TOpt TGNode
genTGOpt 0 _ = []
genTGOpt i (m,tr) = ( (getTransitionGraph [] (tempOpt m) tr, False) : genTGOpt (i-1) (m,tr) )


--Petri anpassen 
-- momentan ohne Funktion
altPetri :: Petri -> TGHelp
altPetri (m,tr) = TGHelp [m] tr

tempOpt :: Mark -> [Mark]
tempOpt m = [map (1+) m]

--CallAlloy Tests