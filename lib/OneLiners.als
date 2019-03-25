module OneLiner

open PetriConstraints

pred tokenChangeSum[n : Int]{
  totalTokenChange[Places] = n
}

pred noActivatedTrans[]{
  no t : Transitions | activated[t]
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transitions | t1 != t2 and concurrent[t1 + t2]
}

pred maxPlaces[n : Int]{
  #Places =< n
}

pred maxTransitions[n : Int]{
  #Transitions =< n
}

pred maxWeight[n : Int]{
  all weight : Nodes.flow[Nodes] | weight =< n
}

pred weightChangeSum[n : Int]{
  totalFlowChange[Nodes, Nodes] = n
}

pred presenceSelfLoop[]{
  some p : Places, t : Transitions | selfLoop[p, t]
}

pred presenceSinkTransition[]{
  some t : Transitions | sinkTransition[t]
}

pred presenceSourceTransition[]{
  some t : Transitions | sourceTransition[t]
}

pred presenceConflict[]{
   some t1, t2 : Transitions, p : Places | conflict[t1, t2, p]
}
