module scenario2

open global

fact{
  Place.defaultTokens in 0
  no flowChange
}

//altogether exactly n tokens should be added
pred nTokensAdded[n : Int]{
  (sum p : Place | p.tokenChange) = n
}

//In each place, at most m tokens should be added
pred mTokensAtMost[m : Int]{
  all p : Place | p.tokenChange =< m
}

//A certain number k of transitions are activated
pred kTransitionsActivated[ts : set Transition, k : Int]{
  #ts = k
  all t : ts | activated[t]
  no t : (Transition - ts) | activated[t]
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transition | concurrency[t1,t2]
}
