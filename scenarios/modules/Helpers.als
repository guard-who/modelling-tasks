module Helpers

open PetriSignature

//sum of tokenChange
fun totalTokenChange[place : set Places] : Int{
  sum p : place | p.tokenChange
}

//total number of flows going from set of nodes to set of nodes
fun totalFlow[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flow[t]
}

//total number of default flows going out from set of nodes to a set of nodes
fun totalDefaultFlow[from, to : set Nodes] : Int{
  sum f : from, t : to | f.defaultFlow[t]
}

//total number of flow changes going out from set of nodes to a set of nodes
fun totalFlowChange[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flowChange[t]
}
