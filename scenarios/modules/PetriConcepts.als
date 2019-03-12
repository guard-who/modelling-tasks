module PetriConcepts

open PetriSignature
open Helpers

pred activated[t : Transitions]{
  all p : Places | p.tokens >= p.flow[t]
}

pred conflict[t1, t2 : Transitions, p : Places]{
  t1 != t2
  activated[t1]
  activated[t2]
  p.tokens < plus[p.flow[t1], p.flow[t2]]
}

pred concurrent[ts : set Transitions]{
  all p : Places | p.tokens >= totalFlow[p, ts]
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
  all p : Places | p.defaultTokens >= totalDefaultFlow[p, ts]
}

pred selfLoop[p : Places, t : Transitions]{
  (one p.flow[t]) and (one t.flow[p])
}

pred sinkTransition[t : Transitions]{
  no t.flow
}

pred sourceTransition[t : Transitions]{
  no Places.flow[t]
}
