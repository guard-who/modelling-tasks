{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module FalsePetri where

import Data.String.Interpolate
import PetriAlloy               (modulePetriSignature)
import Types

testFalse :: Petri -> IO()
testFalse petri = writeFile "output.txt" $ renderFalse petri

renderFalse :: Petri -> String
renderFalse Petri{startM,trans} = [i| module FalseNet

#{modulePetriSignature}

#{givPlaces (length startM)}
#{givTrans (length trans)}

fact{
  #{startMark 1 startM}
  
  #{defFlow 1 trans}
}
|]

givPlaces :: Int -> String
givPlaces 0 = ""
givPlaces p = "one sig S"++show p++" extends givenPlaces{} \n"++ givPlaces (p-1)

givTrans :: Int -> String
givTrans 0 = ""
givTrans t = "one sig T"++show t++" extends givenTransitions{} \n"++ givTrans (t-1)

startMark ::Int -> Mark -> String
startMark _ []      = ""
startMark iM (m:rm) ="S"++ show iM ++".defaultTokens = "++ show m ++"\n  " ++ startMark (iM+1) rm

defFlow :: Int -> [Trans] -> String
defFlow _ []            = ""
defFlow iT ((pr,po):rt) = flowPre iT 1 pr ++ flowPost iT 1 po ++ defFlow (iT+1) rt

flowPre :: Int -> Int -> [Int] -> String
flowPre _ _ [] = ""
flowPre iT iM (m:rm) 
 | m == 0     = "no S"++ show iM ++".defaultFlow[T"++ show iT ++"]\n  " ++ flowPre iT (iM+1) rm
 | otherwise  = "S"++ show iM ++".defaultFlow[T"++ show iT ++"] = "++ show m ++"\n  "
                       ++ flowPre iT (iM+1) rm

flowPost :: Int -> Int -> [Int] -> String
flowPost _ _ [] = ""
flowPost iT iM (m:rm) 
 | m == 0     = "no T"++ show iT ++".defaultFlow[S"++ show iM ++"]\n  " ++ flowPost iT (iM+1) rm
 | otherwise  = "T"++ show iT ++".defaultFlow[S"++ show iM ++"] = "++ show m ++"\n  "
                        ++ flowPost iT (iM+1) rm
