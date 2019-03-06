module scenario5

open global

fact{
  no Place.tokenChange
}


pred maxPlaces[n : Int]{
  #Place =< n
}

pred maxTransitions[n : Int]{
  #Transition =< n
}

pred maxTokens[overall, eachPlace : Int]{
  all p : Place | p.tokens =< eachPlace
  (sum p : Place | p.tokens) =< overall
}

pred maxWeight[n : Int]{
  all weight : Node.flow[Node] | weight =< n
}

pred presenceSelfLoop[]{
  some p : Place, t : Transition | (#(p.flow[t]) = 1) and (#(t.flow[p]) = 1)
}

pred presenceSinkTransition[]{
  some t : Transition | (#t.flow[Place]) = 0
}

pred presenceSourceTransition[]{
  some t : Transition | (#Place.flow[t]) = 0
}

pred numberActivatedTransition[n : Int, ts : set Transition]{
  #ts = n
  all t : ts | activated[t]
  no t : (Transition - ts) | activated[t]
}

pred presenceConflict[]{
   some t1, t2 : Transition | conflict[t1,t2]
}

pred presenceConcurrency[]{
  some ts : set Transition | #ts > 1 and concurrencyMultiple[ts]
}

