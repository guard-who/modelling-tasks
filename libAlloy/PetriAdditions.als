/*
 * Creates petri nets or reconstructs petri net by adding new places or transitions. For reconstructing petri net,
 * predicate noChangesToGivenParts will ensure that no change on the given petri net. That is, no token added on given places,
 * no flow added between given places and given transitions. 
 * It is forbidden to reconstruct a ordered petri net, only non-ordered petri net is possible.
*/

//Places and Transitions to be added
sig addedPlaces extends Places{}
{
  defaultTokens = 0
  no defaultFlow
}

sig addedTransitions extends Transitions{}
{
  no defaultFlow
}


pred noChangesToGivenParts[]{
  no givenPlaces.tokenChange
  let givenNodes = givenPlaces + givenTransitions | no givenNodes.flowChange[givenNodes]
}
