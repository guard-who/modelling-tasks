open PetriConstraints
open OneLiner

fact{
  Place.defaultTokens in 0
  no defaultFlow
}

pred showPetr1[ts : set Transition]{
  maxPlaces[3]
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  maxTransitions[3]
  maxWeight[1]
  numberActivatedTransition[3,ts]
  not presenceSinkTransition
  not presenceSourceTransition
  presenceConflict
  not noConcurrency
  not presenceSelfLoop
}
run showPetr1 for 6
