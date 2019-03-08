open predGlobal

fact{
  Place.defaultTokens in 0
  no flowChange
}

//concrete Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.flow[T1] = 1
  S1.flow[T2] = 1
  S1.flow[T3] = 1

  S2.flow[T2] = 1
  no S2.flow[Transition - T2]

  S3.flow[T2] = 1
  no S3.flow[Transition - T2]

  T1.flow[S2] = 1
  no T1.flow[Place - S2]

  no T2.flow[Place]

  T3.flow[S3] = 1
  no T3.flow[Place - S3]
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
  numberActivatedTransition[2,ts]
}
run showAdd3Mostly2and2TransitionActivated for 3

//exactly 3 tokens added in total, at most 2 for each place, and there is no conflict
pred showAdd3Mostly2NoConflict[]{
  nTokensAdded[3]
  mTokensAtMost[2]
  no t1, t2 : Transition | conflict[t1,t2]
}
run showAdd3Mostly2NoConflict for 3

//exactly 3 tokens added in total, at most 2 for each place, and there are no concurrently activated transitions
pred showAdd3Mostly2NoConcurrency[]{
  nTokensAdded[3]
  mTokensAtMost[2]
  noConcurrency
}
run showAdd3Mostly2NoConcurrency for 3
