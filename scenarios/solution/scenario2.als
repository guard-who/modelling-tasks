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

pred concurrency[t1,t2 : one Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  not conflict[t1,t2]
}

pred concurrencyMultiple[ts : set Transition]{
  all p : one Place | p.tokens >= (sum t : ts | p.inp[t])
}

//altogether exactly n tokens should be added
pred nTokensAdded[n : one Int]{
  n = (sum p : Place | p.tokens)
} 

//In each place, at most m tokens should be added
pred mTokensAtMost[m : one Int]{
  all p : one Place | p.tokens =< m
}

//A certain number k of transitions are activated
pred kTransitionsActivated[ts : set Transition, k : one Int]{
  #ts = k
  all t : one ts | activated[t]
  no t : one (Transition - ts) | activated[t]
}

//there is no concurrently activated transitions
pred noConcurrency[]{
  no t1,t2 : one Transition | concurrency[t1,t2]
}

//concrete Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.inp[T1] = 1
  S1.inp[T2] = 1
  S1.inp[T3] = 1

  S2.inp[T2] = 1
  no S2.inp[Transition - T2]

  S3.inp[T2] = 1
  no S3.inp[Transition - T2]

  T1.out[S2] = 1
  no T1.out[Place - S2]

  no T2.out[Place]

  T3.out[S3] = 1
  no T3.out[Place - S3]
}

//show petri net
pred show[]{
}
run show for 3

//exactly 3 tokens added in total, at most 2 for each place, and T1 activated
pred showAdd3Mostly2T1Activated[]{
  nTokensAdded[3]
  mTokensAtMost[2]
  activated[T1]
}
run showAdd3Mostly2T1Activated for 3

//exactly 3 tokens added in total, at most 2 for each place, and exactly 2 transitions activated
pred showAdd3Mostly2and2TransitionActivated[ts : set Transition]{
  nTokensAdded[3]
  mTokensAtMost[2]
  kTransitionsActivated[ts,2]
}
run showAdd3Mostly2and2TransitionActivated for 3

//exactly 3 tokens added in total, at most 2 for each place, and there is no conflict
pred showAdd3Mostly2NoConflict[]{
  nTokensAdded[4]
  mTokensAtMost[2]
  no t1,t2 : one Transition | conflict[t1,t2]
}
run showAdd3Mostly2NoConflict for 3

//exactly 3 tokens added in total, at most 2 for each place, and there are no concurrently activated transitions
pred showAdd3Mostly2NoConcurrency[]{
  nTokensAdded[3]
  mTokensAtMost[2]
  noConcurrency
}
run showAdd3Mostly2NoConcurrency for 3
