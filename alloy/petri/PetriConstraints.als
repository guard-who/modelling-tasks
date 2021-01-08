module PetriConstraints

open PetriConcepts
open Helpers

//check if maximal set of concurrent transitions
pred maximallyConcurrent[ts : set Transitions]{
  concurrent[ts]
  no t : (Transitions - ts) | concurrent[ts+t]
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

pred theActivatedDefaultTransitions[ts : set givenTransitions]{
  all t : ts | activatedDefault[t]
  no t : (givenTransitions - ts) | activatedDefault[t]
}

pred noIsolatedNodes[]{
  all n : Nodes | some n.flow.Int or some n.~(flow.Int)
}

pred defaultNoIsolatedNodes[]{
  all n : givenNodes | some n.defaultFlow.Int or some n.~(defaultFlow.Int)
}

pred graphIsConnected[]{
  all n : Nodes | Nodes = n + n.^(flow.Int + ~(flow.Int))
}

pred defaultGraphIsConnected[]{
  all n : givenNodes | givenNodes = n + n.^(defaultFlow.Int + ~(defaultFlow.Int))
}
