open PetriNetA
open predGlobal

fact{
  no tokenChange
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred showAddOneWeightOnePairConcurrency[]{
  nWeightAdded[1]
  concurrent[T1 + T3]
}
run showAddOneWeightOnePairConcurrency for 3

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred showRemoveOneWeightOnePairConcurrency[]{
  nWeightRemoved[1]
  concurrent[T1 + T3]
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
