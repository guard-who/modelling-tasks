module scenarios/scenario6
open scenarios/OneLiners

open scenarios/examples/PetriNetD
open lib/PetriConstraints
open lib/PetriAdditions

pred show3Place4Transition[]{
  #Places =< 3
  #Transitions =< 4
  tokensAddedOverall[3]
  perPlaceTokensAddedAtMost[2]
  weightAddedOverall[4]
  not presenceSelfLoop
  not presenceSinkTransition
  not presenceSourceTransition
   presenceConflict
  not noConcurrency
}

run show3Place4Transition for 7

pred show3Place4TransitionFineGrained[]{
  noChangesToGivenParts
  #Places =< 3
  #Transitions =< 4
  tokensAddedOverall[2]
  perPlaceTokensAddedAtMost[2]
  weightAddedOverall[2]
  not presenceSelfLoop
  not presenceSinkTransition
  not presenceSourceTransition
  not presenceConflict
  noConcurrency
}

run show3Place4TransitionFineGrained for 7
  
