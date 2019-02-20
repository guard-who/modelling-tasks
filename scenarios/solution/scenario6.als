abstract sig Place
{
  inp : Transition set -> lone Int,
  defaultInp : Transition set -> lone Int,
  inpChange : Transition set -> lone Int,
  defaultTokens : one Int,
  tokenChange : one Int,
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
  tokens >= 0
}

abstract sig defaultPlace extends Place{}
{
  no defaultInp[transitionChange]
  all t : one Transition | inp[t] = plus[defaultInp[t], inpChange[t]]
}

sig placeChange extends Place{}
{
  no defaultInp[Transition]
  all t : one Transition | inp[t] = inpChange[t]
  defaultTokens = 0
}

abstract sig Transition
{
  out : Place set -> lone Int,
  defaultOut : Place set -> lone Int,
  outChange : Place set -> lone Int
}

abstract sig defaultTransition extends Transition{}
{
  no defaultOut[placeChange]
  all p : one Place | out[p] = plus[defaultOut[p], outChange[p]]
}

sig transitionChange extends Transition{}
{
  no defaultOut[Place]
  all p : one Place | out[p] = outChange[p]
}

fact {
  all weight : one Place.inp[Transition] | weight >= 0
  all weight : one Transition.out[Place] | weight >= 0
}

//default petri net
one sig S1 extends defaultPlace{}
one sig S2 extends defaultPlace{}
one sig T1 extends defaultTransition{}
one sig T2 extends defaultTransition{}
one sig T3 extends defaultTransition{}

fact{
  S1.defaultTokens = 0
  S2.defaultTokens = 1

  S1.defaultInp[T1] = 1
  S1.defaultInp[T2] = 1
  S1.defaultInp[T3] = 1

  S2.defaultInp[T2] = 1
  no S2.defaultInp[Transition - T2]

  T1.defaultOut[S2] = 1
  no T1.defaultOut[Place - S2]

  no T2.defaultOut[Place]

  no T3.defaultOut[Place]  
}
pred activated[t : one Transition]{
  all p : one Place | p.tokens >= p.inp[t]
}

pred conflict[t1,t2 : one Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : one Place | one p.inp[t1] and one p.inp[t2] and p.tokens < plus[p.inp[t1], p.inp[t2]]
}

pred concurrencyMultiple[ts : set Transition]{
  all p : one Place | p.tokens >= (sum t : ts | p.inp[t])
}


pred maxPlacesAdded[n : one Int]{
  #placeChange =< n
}

pred maxTransitionsAdded[n : one Int]{
  #transitionChange =< n
}

pred maxTokens[overall, eachPlace : one Int]{
  all p : one Place | p.tokens =< eachPlace
  (sum p : Place | p.tokens) =< overall
}

pred maxWeightAdded[weight : one Int, flow : one Int]{
  all p : one Place, t : one Transition | p.inpChange[t] >= 0 and t.outChange[p] >= 0
  plus[#(Place.inpChange), #(Transition.outChange)] =< flow
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] =< weight
}

pred maxWeightRemoved[weight : one Int, flow : one Int]{
  all p : one Place, t : one Transition | p.inpChange[t] =< 0 and t.outChange[p] =< 0
  plus[#(Place.inpChange), #(Transition.outChange)] =< flow
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] =< weight
  all p : one Place, t : one Transition | p.inp[t] >= 0 and t.out[p] >= 0
}

pred presenceSelfLoop[]{
  some p : one Place, t : one Transition | p.inp[t] >= 1 and t.out[p] >= 1
}

pred presenceSinkTransition[]{
  some t : one Transition | t.out[Place] = 0
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

pred presenceConcurrency[ts : set Transition]{
  some ts : set Transition | #ts >= 2 and concurrencyMultiple[ts]
}

pred showPetr1[ts : set Transition, tss : set Transition]{
  maxPlacesAdded[2]
  maxTransitionsAdded[1]
  maxTokens[5,2]
  maxWeightAdded[4,3]
  numberActivatedTransition[3,ts]
  not presenceSelfLoop
  not presenceSinkTransition
  not presenceSourceTransition
  presenceConflict
  presenceConcurrency[tss]
}
run showPetr1 for 6
