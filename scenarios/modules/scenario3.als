module scenario3

open global

fact{
  no flowChange
}

//Add exactly one token somewhere so that two transitions are concurrently activated
pred addOneTokenOnePairConcurrency[t1, t2 : Transition]{
  all p : Place | p.tokenChange >= 0
  (sum p : Place | p.tokenChange) = 1
  concurrency[t1,t2]
}

//Remove a token so that there are no activated transitions.
pred removeOneTokenNoActivatedTransition[]{
  all p : Place | p.tokenChange =< 0
  (sum p : Place | p.tokenChange) = (-1)
  no t : Transition | activated[t]
}

//Remove a token so that 2 previously concurrently activated transitions get into conflict
pred removeOneTokenIntoConflict[t1, t2 : Transition]{
  all p : Place | p.defaultTokens >= plus[p.flow[t1], p.flow[t2]]
  all p : Place | p.tokenChange =< 0
  (sum p : Place | p.tokenChange) = (-1)
  conflict[t1,t2]
}
