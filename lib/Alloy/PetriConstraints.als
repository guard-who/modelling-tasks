module PetriConstraints

open PetriConcepts

//set tokens should be added to a petri net only
pred tokenAddOnly[]{
  all tc : Places.tokenChange | tc > 0
}

//set tokens should be removed from a petri net only
pred tokenRemoveOnly[]{
  all tc : Places.tokenChange | tc < 0
}

//check if maximal set of concurrent transitions
pred maximallyConcurrent[ts : set Transitions]{
  concurrent[ts]
  no t : (Transitions - ts) | concurrent[ts+t]
}

pred tokenChangeOverall[n : Int]{
  tokenChangeSum[Places] = n
}

//set weight can be added to a petri net only
pred weightAddOnly[]{
  all change : Nodes.flowChange[Nodes] | change > 0
}

//set weight can be removed from a petri net only
pred weightRemoveOnly[]{
  all change : Nodes.flowChange[Nodes] | change < 0
}

pred flowChangeOverall[n : Int]{
  flowChangeSum[Nodes,Nodes] = n
}

pred theActivatedTransitions[ts : set Transitions]{
  all t : ts | activated[t]
  no t : (Transitions - ts) | activated[t]
}
