module PetriSignature

abstract sig Nodes
{
  flow : Nodes -> lone Int,
  defaultFlow : Nodes -> lone Int,
  flowChange : Nodes -> lone (Int - 0)
}
{
  all weight : defaultFlow[Nodes] | weight > 0
  all n : Nodes | let theFlow = plus[defaultFlow[n], flowChange[n]] | theFlow = 0 implies no flow[n] else flow[n] = theFlow
}

abstract sig Places extends Nodes
{
  defaultTokens : one Int,
  tokenChange : lone (Int - 0),
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
  tokens >= 0
  defaultFlow.Int in Transitions
}

abstract sig Transitions extends Nodes
{
}
{
  defaultFlow.Int in Places
}

fact isLegalPetrinet {
  all weight : Nodes.flow[Nodes] | weight > 0
  Places.flow.Int in Transitions
  Transitions.flow.Int in Places
}

//set default places and transitions
abstract sig givenPlaces extends Places{}
abstract sig givenTransitions extends Transitions{}
