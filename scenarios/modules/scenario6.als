module scenario6

open PetriNetD
open PetriConstraints
open PetriAdditions

pred show3Place4Transition[]{
  maxPlaces[3]
  maxTransitions[4]
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
  
