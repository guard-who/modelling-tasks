module Compatibility

open PetriConstraints

//check if maximum set of concurrent transitions
pred isMaxConcurrency[ts : set Transitions]{
  maximallyConcurrent[ts]
}

//altogether exactly n transitions should be activated
pred numberActivatedTransition[n : Int, ts : set Transitions]{
  #ts = n
  theActivatedTransitions[ts]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  tokenChangeOverall[n]
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  tokenChangeOverall[minus[0,n]]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Places | p.tokenChange =< m
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  flowChangeOverall[n]
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  flowChangeOverall[minus[0,n]]
}
