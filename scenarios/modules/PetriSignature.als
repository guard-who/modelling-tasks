module PetriSignature

abstract sig Node
{
  flow : Node -> lone Int,
  defaultFlow : Node -> lone Int,
  flowChange : Node -> lone (Int - 0)
}
{
  all weight : defaultFlow[Node] + flow[Node] | weight > 0
  all n : Node | let theFlow = plus[defaultFlow[n], flowChange[n]] | theFlow = 0 implies no flow[n] else flow[n] = theFlow
}

abstract sig Place extends Node
{
  defaultTokens : one Int,
  tokenChange : lone (Int - 0),
  tokens : one Int
}
{
  defaultTokens >= 0
  tokens = plus[defaultTokens, tokenChange]
  tokens >= 0
  //set place only going to transition
  flow.Int in Transition
  defaultFlow.Int in Transition
}

abstract sig Transition extends Node
{
}
{
  //set transition only going to place
  flow.Int in Place
  defaultFlow.Int in Place
}

//set default places and transitions
abstract sig givenPlace extends Place{}
abstract sig givenTransition extends Transition{}
