open PetriNetC_Ordered
open PetriConstraints
open OneLiner

fact{
  no flowChange
}

//show petri net
pred show[]{
}
run show for 3

//exactly 3 tokens added in total, at most 2 for each place, and T1 activated
pred showAdd3Mostly2T1Activated[]{
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  activated[T1]
}
run showAdd3Mostly2T1Activated for 3

//exactly 3 tokens added in total, at most 2 for each place, and exactly 2 transitions activated
pred showAdd3Mostly2and2TransitionActivated[ts : set Transitions]{
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  numberActivatedTransition[2,ts]
}
run showAdd3Mostly2and2TransitionActivated for 3

//exactly 3 tokens added in total, at most 2 for each place, and there is no conflict
pred showAdd3Mostly2NoConflict[]{
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  not presenceConflict
}
run showAdd3Mostly2NoConflict for 3

//exactly 3 tokens added in total, at most 2 for each place, and there are no concurrently activated transitions
pred showAdd3Mostly2NoConcurrency[]{
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  noConcurrency
}
run showAdd3Mostly2NoConcurrency for 3
