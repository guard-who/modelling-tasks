module PetriConstraints

open PetriConcepts

pred tokenAddOnly[]{
  all tc : Place.tokenChange | tc > 0
}

pred tokenRemoveOnly[]{
  all tc : Place.tokenChange | tc < 0
}

pred isMaxConcurrency[ts : set Transition]{
  concurrent[ts]
  no t : (Transition - ts) | concurrent[ts+t]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  totalTokenChange[Place] = n
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  totalTokenChange[Place] = minus[0,n]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Place | p.tokenChange =< m
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

pred numberActivatedTransition[n : Int, ts : set Transition]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transition - ts) | activated[t]
}
