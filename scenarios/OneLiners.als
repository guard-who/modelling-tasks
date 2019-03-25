module scenarios/OneLiners

open lib/PetriConstraints

pred noActivatedTrans[]{
  no t : Transitions | activated[t]
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transitions | t1 != t2 and concurrent[t1 + t2]
}

pred maxWeight[n : Int]{
  all weight : Nodes.flow[Nodes] | weight =< n
}

pred weightChangeSum[n : Int]{
  flowChangeSum[Nodes, Nodes] = n
}

pred presenceSelfLoop[]{
  some p : Places, t : Transitions | selfLoop[p, t]
}

pred presenceSinkTransition[]{
  some t : Transitions | sinkTransitions[t]
}

pred presenceSourceTransition[]{
  some t : Transitions | sourceTransitions[t]
}

pred presenceConflict[]{
   some t1, t2 : Transitions, p : Places | conflict[t1, t2, p]
}
