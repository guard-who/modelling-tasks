module PetriAdditions

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
