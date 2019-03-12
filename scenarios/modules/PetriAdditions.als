module PetriAdditions

/*
 * Creates petri nets or reconstructs petri net by adding new places or transitions. For reconstructing petri net,
 * predicate fineGrained will ensure that no change on the given petri net. That is, no token added on given places,
 * no flow added between given places and given transitions. 
 * It is forbidden to reconstruct a ordered petri net, only non-ordered petri net is possible.
*/

open PetriSignature

//Place and Transition to be added
sig addedPlace extends Places{}
{
  defaultTokens in 0
  no defaultFlow
}
sig addedTransition extends Transitions{}
{
  no defaultFlow
}

pred fineGrained[]{
  givenPlaces.tokenChange in 0
  no givenPlaces.flowChange[givenTransitions]
  no givenTransitions.flowChange[givenPlaces]
}
