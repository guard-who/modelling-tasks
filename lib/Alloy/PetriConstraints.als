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

//set weight can be added to a petri net only
pred weightAddOnly[]{
  all change : Nodes.flowChange[Nodes] | change > 0
}

//set weight can be removed from a petri net only
pred weightRemoveOnly[]{
  all change : Nodes.flowChange[Nodes] | change < 0
}

pred maxTokenChangePerPlace[max : Int] {
  no place : Places | abs[place.tokenChange] > max
}

pred maxFlowChangePerEdge[max : Int] {
  no n, m : Nodes | abs[n.flowChange[m]] > max
}

pred theActivatedTransitions[ts : set Transitions]{
  all t : ts | activated[t]
  no t : (Transitions - ts) | activated[t]
}

pred theActivatedDefaultTransitions[ts : set Transitions]{
  all t : ts | activatedDefault[t]
  no t : (Transitions - ts) | activatedDefault[t]
}

pred noIsolatedNodes[]{
  all n : Nodes | some n.flow.Int or some n.~(flow.Int)
}

pred graphIsConnected[]{
  all n : Nodes | Nodes = n + n.^(flow.Int + ~(flow.Int))
}

pred defaultGraphIsConnected[]{
  all n : Nodes | Nodes = n + n.^(defaultFlow.Int + ~(defaultFlow.Int))
}
