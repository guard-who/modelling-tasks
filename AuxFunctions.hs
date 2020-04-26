module AuxFunctions where
--Auxiliary Functions--Hilfsfunktionen--

--extract from Triples
extractTFirst :: (a,b,c) -> a
extractTFirst (a,_,_) = a

extractTSecond :: (a,b,c) -> b
extractTSecond (_,b,_) = b

extractTThird :: (a,b,c) -> c
extractTThird (_,_,c) = c