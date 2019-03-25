module scenarios/scenario1
open scenarios/examples/PetriNetB_Ordered
open lib/PetriConstraints

fact{
  no tokenChange
  no flowChange
}

//which transitions are activated
pred showActivated[t : Transitions]{
  activated[t]
}

//transitions in conflict, duplicated results removed
pred showConf[t1, t2 : Transitions]{
  gt[t2,t1]
  some placeConflict : Places | conflict[t1, t2, placeConflict]
}

//multiple transitions concurrently activated
pred showMultipleCon[ts : set Transitions]{
  #ts > 1
  concurrent[ts]
}

//max concurrently activated
pred showMax[ts : set Transitions]{
  isMaxConcurrency[ts]
}


run showActivated for 3

run showConf for 3

run showMultipleCon for 3

run showMax for 3
