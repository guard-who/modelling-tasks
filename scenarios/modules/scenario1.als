module moduleScenario1

open scenario12 as s12
open util/ordering[Transition]

pred isMaxConcurrency[ts : set Transition]{
  concurrencyMultiple[ts]
  no t : (Transition - ts) | concurrencyMultiple[ts+t]
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
  concurrencyMultiple[ts]
}

//max concurrently activated
pred showMax[ts : set Transition]{
  isMaxConcurrency[ts]
}

