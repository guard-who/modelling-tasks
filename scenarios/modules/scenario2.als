module scenario2

open global

fact{
  no tokenChange
  no flowChange
}

//altogether exactly n tokens should be added
pred nTokensAdded[n : Int]{
  n = (sum p : Place | p.tokens)
} 

//In each place, at most m tokens should be added
pred mTokensAtMost[m : Int]{
  all p : Place | p.tokens =< m
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
