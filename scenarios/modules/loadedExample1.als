open PetriNetB
open predGlobal
open util/ordering[PetriNetB/Transition]

fact{
  no tokenChange
  no flowChange
}

fact {
  //set the order for transitions
  no T1.prevs
  T1.next = T2
  T2.next = T3
  no T3.next
}

//which transitions are activated
pred showActivated[t : Transition]{
  activated[t]
}

//transitions in conflict, duplicated results removed
pred showConf[t1, t2 : Transition]{
  gt[t2,t1]
  conflict[t1,t2]
}

//multiple transitions concurrently activated
pred showMultipleCon[ts : set Transition]{
  #ts > 1
  concurrent[ts]
}

//max concurrently activated
pred showMax[ts : set Transition]{
  isMaxConcurrency[ts]
}


run showActivated for 3

run showConf for 3

run showMultipleCon for 3

run showMax for 3
