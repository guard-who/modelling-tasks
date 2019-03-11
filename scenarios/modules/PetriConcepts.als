module PetriConcepts

open PetriSignature

pred activated[t : Transition]{
  all p : Place | p.tokens >= p.flow[t]
}

pred conflict[t1, t2 : Transition, p : Place]{
  t1 != t2
  activated[t1]
  activated[t2]
  p.tokens < plus[p.flow[t1], p.flow[t2]]
}

pred concurrent[ts : set Transition]{
  all p : Place | p.tokens >= (sum t : ts | p.flow[t])
}

//check activation under default condition
 pred activatedDefault[t : Transition]{
    all p : Place | p.defaultTokens >= p.defaultFlow[t]
}

//check conflict under default condition
pred conflictDefault[t1, t2 : Transition, p : Place]{
  t1 != t2
  activatedDefault[t1]
  activatedDefault[t2]
  p.defaultTokens < plus[p.defaultFlow[t1], p.defaultFlow[t2]]
}

//check concurrent under default condition
pred concurrentDefault[ts : set Transition]{
  all p : Place | p.defaultTokens >= (sum t : ts | p.defaultFlow[t])
}

pred selfLoop[p : Place, t : Transition]{
  (one p.flow[t]) and (one t.flow[p])
}

pred sinkTransition[t : Transition]{
  no t.flow
}

pred sourceTransition[t : Transition]{
  no Place.flow[t]
}
