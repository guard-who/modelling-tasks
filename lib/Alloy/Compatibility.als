module Compatibility

open PetriConstraints

//check if maximum set of concurrent transitions
pred isMaxConcurrency[ts : set Transitions]{
  maximallyConcurrent[ts]
}

//altogether exactly n transitions should be activated
pred numberActivatedTransition[n : Int, ts : set Transitions]{
  #ts = n
  theActivatedTransitions[ts]
}

fun tokenSum[places : set Places] : Int{
  sum p : places | p.tokens
}

fun defaultTokenSum[places : set givenPlaces] : Int{
  sum p : places | p.defaultTokens
}

//sum of tokenChange
fun tokenChangeSum[places : set Places] : Int{
  sum p : places | p.tokenChange
}

fun tokenChangeAbsolutesSum[places : set Places] : Int{
  sum p : places | abs[p.tokenChange]
}

fun flowChangeAbsolutesSum[from, to : set Nodes] : Int{
  sum f : from, t : to | abs[f.flowChange[t]]
}

//set tokens should be added to a petri net only
pred tokenAddOnly[]{
  all tc : Places.tokenChange | tc > 0
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  tokenChangeSum[Places] = n
}

//set tokens should be removed from a petri net only
pred tokenRemoveOnly[]{
  all tc : Places.tokenChange | tc < 0
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  tokenChangeSum[Places] = minus[0,n]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Places | p.tokenChange =< m
}

//total number of flows going from set of nodes to set of nodes
fun flowSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flow[t]
}

//total number of flow changes going out from set of nodes to a set of nodes
fun flowChangeSum[from, to : set Nodes] : Int{
  sum f : from, t : to | f.flowChange[t]
}

//total number of default flows going out from set of nodes to a set of nodes
fun defaultFlowSum[from, to : set (givenPlaces + givenTransitions)] : Int{
  sum f : from, t : to | f.defaultFlow[t]
}

//set weight can be added to a petri net only
pred weightAddOnly[]{
  all change : Nodes.flowChange[Nodes] | change > 0
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  flowChangeSum[Nodes,Nodes] = n
}

//set weight can be removed from a petri net only
pred weightRemoveOnly[]{
  all change : Nodes.flowChange[Nodes] | change < 0
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  flowChangeSum[Nodes,Nodes] = minus[0,n]
}

//check if there is a loop between a place and a transition
// pred selfLoop[p : Places, t : Transitions]{
//   (one p.flow[t]) and (one t.flow[p])
// }
