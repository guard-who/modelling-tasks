module scenarios/scenario4
open scenarios/examples/PetriNetA_Ordered
open lib/PetriConstraints
open scenarios/OneLiners

fact{
  no tokenChange
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred showAddOneWeightOnePairConcurrency[]{
  weightAddedOverall[1]
  concurrent[T1 + T3]
}
run showAddOneWeightOnePairConcurrency for 3

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred showRemoveOneWeightOnePairConcurrency[]{
  weightRemovedOverall[1]
  concurrent[T1 + T3]
}
run showRemoveOneWeightOnePairConcurrency for 3

//Add exactly one weight somewhere so that no transitions is activated
pred showAddOneWeightNoActivatedTrans[]{
  weightAddedOverall[1]
  noActivatedTrans
}
run  showAddOneWeightNoActivatedTrans for 3

//Remove exactly one weight somewhere so that no transitions is activated
pred showRemoveOneWeightNoActivatedTrans[]{
  weightRemovedOverall[1]
  noActivatedTrans
}
run  showRemoveOneWeightNoActivatedTrans for 3

//Add exactly one weight somewhere so that two previously concurrently activated transitions get into conflict
pred showAddOneWeightConcurrentTransIntoConflict[]{
  concurrentDefault[T1 + T3]
  weightAddedOverall[1]
  some placeConflict : Places | conflict[T1, T3, placeConflict]
}
run showAddOneWeightConcurrentTransIntoConflict for 3
