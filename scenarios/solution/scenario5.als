abstract sig Place
{
  inp : Transition -> lone Int,
  tokens : one Int
}
{
  tokens >= 0
}

abstract sig Transition
{
  out : Place -> lone Int
}

fact {
  let Node = Place + Transition |
  let flow = inp + out |
  all weight : Node.flow[Node] | weight > 0
}

pred activated[t : Transition]{
  all p : Place | p.tokens >= p.inp[t]
}

pred conflict[t1, t2 : Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : Place | p.tokens < plus[p.inp[t1], p.inp[t2]]
}

pred concurrencyMultiple[ts : set Transition]{
  all p : Place | p.tokens >= (sum t : ts | p.inp[t])
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
  let Node = Place + Transition |
  let flow = inp + out |
  all weight : Node.flow[Node] | weight =< n
}

pred presenceSelfLoop[]{
  some p : Place, t : Transition | (#(p.inp[t]) = 1) and (#(t.out[p]) = 1)
}

pred presenceSinkTransition[]{
  some t : Transition | (#t.out[Place]) = 0
}

pred presenceSourceTransition[]{
  some t : Transition | (#Place.inp[t]) = 0
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
