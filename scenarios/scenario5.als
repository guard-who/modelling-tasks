module scenarios/scenario5
open lib/PetriConstraints
open lib/PetriAdditions
open scenarios/OneLiners

fact{
 no givenPlaces
  no givenTransitions
}

pred showPetr1[ts : set Transitions]{
  #Places =< 3
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  #Transitions =< 3
  maxWeight[1]
  numberActivatedTransition[3,ts]
  not presenceSinkTransition
  not presenceSourceTransition
  presenceConflict
  not noConcurrency
  not presenceSelfLoop
}
run showPetr1 for 6
