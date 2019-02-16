abstract sig Place
{
  inp : Transition set -> lone Int,
  tokens : one Int
}
{
  tokens >= 0
}

abstract sig Transition
{
  out : Place set -> lone Int
}

fact {
  let Node = Place + Transition |
  let flow = inp + out |
  all weight : one Node.flow[Node] | weight > 0
}

pred activated[t : one Transition]{
  all p : one Place | p.tokens >= p.inp[t]
}

pred conflict[t1,t2 : one Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : one Place | one p.inp[t1] and one p.inp[t2] and p.tokens < add[p.inp[t1] , p.inp[t2]]
}

pred concurrencyMultiple[ts : set Transition]{
  all p : one Place | p.tokens >= (sum t:ts | p.inp[t])
}


pred maxPlaces[n : one Int]{
  #Place >= 0
  #Place =< n
}

pred maxTransitions[n : one Int]{
  #Transition >= 0
  #Transition =< n
}

pred maxTokens[overall, eachPlace : one Int]{
  all p : one Place | p.tokens =< eachPlace
  (sum p : Place | p.tokens) =< overall
}

pred maxWeight[n : one Int]{
  let Node = Place + Transition |
  let flow = inp + out |
  all weight : one Node.flow[Node] | weight =< n
}

pred presenceSelfLoop[]{
  some p : one Place, t : one Transition | (#(p.inp[t]) = 1) and (#(t.out[p]) = 1)
}

pred presenceSinkTransition[]{
  some t : one Transition | (#t.out[Place]) = 0
}

pred presenceSourceTransition[]{
  some t : Transition | (#Place.inp[t]) = 0
}
pred numberActivatedTransition[n : one Int, ts : set Transition]{
  #Transition >= n
  #ts = n
  all t : one ts | activated[t]
  no t : one (Transition - ts) | activated[t]
}

pred presenceConflict[]{
   some t1, t2 : one Transition | (t1 != t2) and conflict[t1,t2]
}

pred presenceConcurrency[]{
  some ts : set Transition | #ts>=2 and concurrencyMultiple[ts]
}

pred showPetr1[ts : set Transition]{
  maxPlaces[3]
  maxTokens[3,2]
  maxTransitions[3]
  maxWeight[1]
  numberActivatedTransition[3,ts]
  not presenceSinkTransition
  not presenceSourceTransition
  presenceConflict
  presenceConcurrency
  not presenceSelfLoop
}
run showPetr1 for 3
