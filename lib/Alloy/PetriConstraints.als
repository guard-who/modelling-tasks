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

pred theActivatedTransitions[ts : set Transitions]{
  all t : ts | activated[t]
  no t : (Transitions - ts) | activated[t]
}

pred noIsolatedNodes[]{
  all n : Nodes | some m : Nodes | m in n.flow.Int or n in m.flow.Int
}

pred graphIsConnected[]{
  all n,m : Nodes | n != m implies n in m.^(flow.Int + ~(flow.Int))
}
