module PetriConstraints

open PetriConcepts

pred tokenAddOnly[]{
  all tc : Places.tokenChange | tc > 0
}

pred tokenRemoveOnly[]{
  all tc : Places.tokenChange | tc < 0
}

pred isMaxConcurrency[ts : set Transitions]{
  concurrent[ts]
  no t : (Transitions - ts) | concurrent[ts+t]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  totalTokenChange[Places] = n
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  totalTokenChange[Places] = minus[0,n]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Places | p.tokenChange =< m
}

pred weightAddOnly[]{
  all change : Nodes.flowChange[Nodes] | change > 0
}

pred weightRemoveOnly[]{
  all change : Nodes.flowChange[Nodes] | change < 0
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  totalFlowChange[Nodes,Nodes] = n
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  totalFlowChange[Nodes,Nodes] = minus[0,n]
}

pred numberActivatedTransition[n : Int, ts : set Transitions]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transitions - ts) | activated[t]
}
