open predGlobal

fact{
  no tokenChange
}

//default Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.tokens = 1
  S2.tokens = 1
  S3.tokens = 0

  S1.defaultFlow[T1] = 1
  S1.defaultFlow[T2] = 1
  S1.defaultFlow[T3] = 1

  S2.defaultFlow[T2] = 1
  no S2.defaultFlow[Transition - T2]

  S3.defaultFlow[T2] = 1
  no S3.defaultFlow[Transition - T2]

  T1.defaultFlow[S2] = 1
  no T1.defaultFlow[Place - S2]

  no T2.defaultFlow[Place]

  T3.defaultFlow[S3] = 1
  no T3.defaultFlow[Place - S3]
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred showAddOneWeightOnePairConcurrency[]{
  nWeightAdded[1]
  concurrency[T1,T3]
}
run showAddOneWeightOnePairConcurrency for 3

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred showRemoveOneWeightOnePairConcurrency[]{
  nWeightRemoved[1]
  concurrency[T1,T3]
}
run showRemoveOneWeightOnePairConcurrency for 3

//Add exactly one weight somewhere so that no transitions is activated
pred showAddOneWeightNoActivatedTrans[]{
  nWeightAdded[1]
  noActivatedTrans
}
run  showAddOneWeightNoActivatedTrans for 3

//Remove exactly one weight somewhere so that no transitions is activated
pred showRemoveOneWeightNoActivatedTrans[]{
  nWeightRemoved[1]
  noActivatedTrans
}
run  showRemoveOneWeightNoActivatedTrans for 3
