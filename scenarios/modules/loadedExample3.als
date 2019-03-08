open PetriNetA
open predGlobal

fact{
  no flowChange
}

//Remove a token so that there are no activated transitions.
pred showAddOneTokenOnePairConcurrency[]{
  nTokensAdded[1]
  concurrent[T1 + T3]
}
run showAddOneTokenOnePairConcurrency for 3

//Remove a token so that there are no activated transitions.
pred showRemoveOneTokenNoActivatedTransition[]{
  nTokensRemoved[1]
  noActivatedTrans
}
run showRemoveOneTokenNoActivatedTransition for 3

//Remove a token so that 2 previously concurrently activated transitions get into conflict
pred showRemoveOneTokenIntoConflict[t1, t2 : Transition]{
  concurrentDefault[t1 + t2]
  nTokensRemoved[1]
  conflict[t1,t2]
}
run showRemoveOneTokenIntoConflict

pred showActivatedDefault[t : Transition]{
  no tokenChange
  activatedDefault[t]
}
run showActivatedDefault for 3

pred showConflictDefault[t1, t2 : Transition]{
  no tokenChange
  conflictDefault[t1,t2]
}
run showConflictDefault for 3
