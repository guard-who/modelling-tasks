module OneLiner

open PetriConstraints

pred tokenChangeSum[n : Int]{
  totalTokenChange[Place] = n
}

pred noActivatedTrans[]{
  no t : Transition | activated[t]
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1, t2 : Transition | t1 != t2 and concurrent[t1 + t2]
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

pred presenceConflict[]{
   some t1, t2 : Transition, p : Place | conflict[t1, t2, p]
}
