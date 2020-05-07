module Helpers

open PetriSignature

fun tokenSum[places : set Places] : Int{
  sum p : places | p.tokens
}

fun defaultTokenSum[places : set Places] : Int{
  sum p : places | p.defaultTokens
}

//sum of tokenChange
fun tokenChangeSum[places : set Places] : Int{
  sum p : places | p.tokenChange
}

fun tokenChangeAbsolutesSum[places : set Places] : Int{
  sum p : places | abs[p.tokenChange]
}

//total number of flows going from set of nodes to set of nodes
fun flowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flow[t]
}

//total number of default flows going out from set of nodes to a set of nodes
fun defaultFlowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.defaultFlow[t]
}

//total number of flow changes going out from set of nodes to a set of nodes
fun flowChangeSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flowChange[t]
}

fun flowChangeAbsolutesSum[from, to : set Nodes] : Int{
  sum f : from, t : to | abs[f.flowChange[t]]
}

fun abs[n : Int] : Int {
  n >= 0 implies n else minus[0, n]
}
