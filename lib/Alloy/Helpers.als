module Helpers

open PetriSignature

fun tokenSum[places : set Places] : Int{
  sum p : places | p.tokens
}

//total number of flows going from set of nodes to set of nodes
fun flowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flow[t]
}

//total number of default flows going out from set of nodes to a set of nodes
fun defaultFlowSum[from, to : set (givenPlaces + givenTransitions)] : Int{
  sum f : from, t : to | f.defaultFlow[t]
}

fun abs[n : Int] : Int {
  n >= 0 implies n else minus[0, n]
}
