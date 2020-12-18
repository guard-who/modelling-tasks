module PetriConcepts

open PetriSignature
open Helpers

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

//check if given transitions are concurrent
pred concurrent[ts : set Transitions]{
  all p : Places | p.tokens >= (sum t : ts | p.flow[t])
}

//check activation under default condition
 pred activatedDefault[t : givenTransitions]{
  all p : givenPlaces | p.defaultTokens >= p.defaultFlow[t]
}

//check conflict under default condition
pred conflictDefault[t1, t2 : givenTransitions, p : givenPlaces]{
  t1 != t2
  activatedDefault[t1]
  activatedDefault[t2]
  p.defaultTokens < plus[p.defaultFlow[t1], p.defaultFlow[t2]]
}

//check concurrent under default condition
pred concurrentDefault[ts : set givenTransitions]{
  all p : givenPlaces | p.defaultTokens >= (sum t : ts | p.defaultFlow[t])
}

//check if there is a loop between two nodes
pred selfLoop[n : Nodes]{
  n in n.flow.Int.flow.Int
}

//check if some transitions are sink transitions
pred sinkTransitions[ts : set Transitions]{
  no ts.flow
}

//check if some transitions are source transitions
pred sourceTransitions[ts : set Transitions]{
  no Nodes.flow[ts]  // under assumption of valid Petri nets, could use Places instead of Nodes here
}
