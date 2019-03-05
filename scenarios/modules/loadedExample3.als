open scenario3

//default Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

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

pred showaddOneTokenOnePairConcurrency[]{
  addOneTokenOnePairConcurrency[T1, T3]
}
run showaddOneTokenOnePairConcurrency for 3

pred showremoveOneTokenNoActivatedTransition[]{
  removeOneTokenNoActivatedTransition[]
}
run showremoveOneTokenNoActivatedTransition for 3

pred showRemoveOneTokenIntoConflict[t1, t2 : Transition]{
  removeOneTokenIntoConflict[t1, t2]
}
run showRemoveOneTokenIntoConflict
