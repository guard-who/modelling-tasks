module PetriConstraints

open PetriConcepts

pred tokenAddOnly[]{
  all tc : Place.tokenChange | tc > 0
}

pred tokenRemoveOnly[]{
  all tc : Place.tokenChange | tc < 0
}

pred tokenChangeSum[n : Int]{
  totalTokenChange[Place] = n
}

pred noActivatedTrans[]{
  no t : Transition | activated[t]
}

pred isMaxConcurrency[ts : set Transition]{
  concurrent[ts]
  no t : (Transition - ts) | concurrent[ts+t]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  tokenChangeSum[n]
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  tokenChangeSum[minus[0,n]]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Place | p.tokenChange =< m
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transition | t1 != t2 and concurrent[t1 + t2]
}

pred weightAddOnly[]{
  all change : Node.flowChange[Node] | change > 0
}

pred weightRemoveOnly[]{
  all change : Node.flowChange[Node] | change < 0
}

pred weightChangeSum[n : Int]{
  totalFlowChange[Node,Node] = n
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  weightChangeSum[n]
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  weightChangeSum[minus[0,n]]
}

pred maxPlaces[n : Int]{
  #Place =< n
}

pred maxTransitions[n : Int]{
  #Transition =< n
}

pred maxWeight[n : Int]{
  all weight : Node.flow[Node] | weight =< n
}

pred presenceSelfLoop[]{
  some p : Place, t : Transition | selfLoop[p, t]
}

pred presenceSinkTransition[]{
  some t : Transition | sinkTransition[t]
}

pred presenceSourceTransition[]{
  some t : Transition | sourceTransition[t]
}

pred numberActivatedTransition[n : Int, ts : set Transition]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transition - ts) | activated[t]
}

pred presenceConflict[]{
   some t1, t2 : Transition, p : Place | conflict[t1, t2, p]
}
