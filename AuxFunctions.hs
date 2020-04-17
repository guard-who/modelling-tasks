module AuxFunctions where
--Auxiliary Functions--Hilfsfunktionen--

import qualified Data.Set as Set

--extract from Triples
extractTFirst :: (a,b,c) -> a
extractTFirst (a,_,_) = a

extractTSecond :: (a,b,c) -> b
extractTSecond (_,b,_) = b

extractTThird :: (a,b,c) -> c
extractTThird (_,_,c) = c

--work with Set
getFirstElem :: Set.Set a -> a
getFirstElem s = Set.elemAt 0 s

--work with Lists
list2nd :: [a] -> a
list2nd (b:c:rs) = c